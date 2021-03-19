{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}


module XMonad.Layout.PseudoTiling (
    pseudoTiling,
    doPseudoTile,
    PseudoTilingMessage (..),
) where


import           Control.Monad                (foldM)
import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           Graphics.X11                 (Rectangle (..))
import           XMonad                       hiding (windows)
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.StackSet              as W
import           XMonad.Util.XUtils           (fi)


type Dimensions = (Dimension, Dimension)


data PseudoTiling window = PseudoTiling
    { preferredDimensions :: Map window Dimensions
    , pseudoWindows       :: Set window
    } deriving (Read)
deriving instance Show window => Show (PseudoTiling window)


data PseudoTilingMessage = ToggleFocusedWindow
                         | ToggleWindow Window
    deriving Typeable

instance Message PseudoTilingMessage


instance LM.LayoutModifier PseudoTiling Window where
    modifyLayoutWithUpdate :: LayoutClass layout Window
                           => PseudoTiling Window
                           -> W.Workspace WorkspaceId (layout Window) Window
                           -> Rectangle
                           -> X (([(Window, Rectangle)],
                                  Maybe (layout Window)),
                                 Maybe (PseudoTiling Window))
    modifyLayoutWithUpdate pseudoTiler workspace screenRectangle = do
        let PseudoTiling oldDimensions pseudoWindows = pseudoTiler
        let windows = W.integrate' $ W.stack workspace
        newDimensions <- foldM (findDimensions oldDimensions) Map.empty windows
        let newPseudoWindows = Set.intersection pseudoWindows (Map.keysSet newDimensions)
        layout <- runLayout workspace screenRectangle
        return (layout, Just (PseudoTiling newDimensions newPseudoWindows))
      where
        findDimensions :: Map Window Dimensions
                       -> Map Window Dimensions
                       -> Window
                       -> X (Map Window Dimensions)
        findDimensions oldDimensions newDimensions window = do
            dimensions <- case Map.lookup window oldDimensions of
                            Just dimensions -> return dimensions
                            Nothing         -> getPreferredDimensions window
            return $ Map.insert window dimensions newDimensions

    pureModifier :: PseudoTiling Window
                 -> Rectangle
                 -> Maybe (W.Stack Window)
                 -> [(Window, Rectangle)]
                 -> ([(Window, Rectangle)], Maybe (PseudoTiling Window))
    pureModifier (PseudoTiling dimensions windows) _ _ windowRectangles =
        let newWindowRectangles = for windowRectangles $ \(window, rectangle) ->
                case Map.lookup window dimensions of
                    Nothing -> (window, rectangle)
                    Just preferredDimension ->
                        if Set.member window windows
                        then pseudoTile preferredDimension (window, rectangle)
                        else (window, rectangle)
         in (newWindowRectangles, Nothing)


    handleMess :: PseudoTiling Window
               -> SomeMessage
               -> X (Maybe (PseudoTiling Window))
    handleMess pseudoTiler message
        | Just (ToggleWindow window) <- fromMessage message =
            return . Just $ toggle window
        | Just ToggleFocusedWindow <- fromMessage message =
            withWindowSet $ return . fmap toggle . W.peek
      where
        toggle :: Window -> PseudoTiling Window
        toggle currentWindow = pseudoTiler{
            pseudoWindows = toggleMember currentWindow (pseudoWindows pseudoTiler)
          }
          where
            toggleMember el set = if Set.member el set
              then Set.delete el set
              else Set.insert el set
    handleMess _ _ = return Nothing


-------------------------------------------------------------------------------
pseudoTiling :: layout window -> LM.ModifiedLayout PseudoTiling layout window
pseudoTiling = LM.ModifiedLayout (PseudoTiling Map.empty Set.empty)


getPreferredDimensions :: Window -> X Dimensions
getPreferredDimensions window = withDisplay $ \display -> do
    attributes <- liftIO $ getWindowAttributes display window
    let preferredWidth = wa_width attributes + 2 * wa_border_width attributes
    let preferredHeight = wa_height attributes + 2 * wa_border_width attributes
    return (fi preferredWidth, fi preferredHeight)


pseudoTile :: Dimensions -> (Window, Rectangle) -> (Window, Rectangle)
pseudoTile (wPreferred, hPreferred) (window, rectangleBefore) =
    let (Rectangle xBefore yBefore wBefore hBefore) = rectangleBefore
        -- don't extend beyond the underlying layout's allocated space
        w = min wPreferred wBefore
        h = min hPreferred hBefore
        -- center
        x = xBefore + fi (wBefore `div` 2) - fi (w `div` 2)
        y = yBefore + fi (hBefore `div` 2) - fi (h `div` 2)
     in (window, rectangleBefore { rect_x = x, rect_y = y, rect_width = w, rect_height = h })


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap


doPseudoTile :: ManageHook
doPseudoTile = do
    window <- ask
    liftX $ broadcastMessage $ ToggleWindow window
    idHook

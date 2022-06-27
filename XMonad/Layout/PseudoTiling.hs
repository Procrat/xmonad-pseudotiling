{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}


module XMonad.Layout.PseudoTiling (
    pseudoTiling,
    doPseudoTile,
    PseudoTilingMessage(..),
) where


import           Data.Map.Strict              (Map)
import qualified Data.Map.Strict              as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           XMonad                       hiding (display, windows)
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.StackSet              as W
import           XMonad.Util.XUtils           (fi)


type Dimensions = (Dimension, Dimension)


data PseudoTiling window = PseudoTiling
    { preferredDimensions :: Map window Dimensions
    , pseudoWindows       :: Set window
    } deriving (Read)
deriving instance Show window => Show (PseudoTiling window)


data PseudoTilingMessage = SetWindow Window
                         | ToggleWindow Window
    deriving Typeable

instance Message PseudoTilingMessage


instance LM.LayoutModifier PseudoTiling Window where
    pureModifier :: PseudoTiling Window
                 -> Rectangle
                 -> Maybe (W.Stack Window)
                 -> [(Window, Rectangle)]
                 -> ([(Window, Rectangle)], Maybe (PseudoTiling Window))
    pureModifier (PseudoTiling dimensions windows) _ _ windowRectangles =
        let newWindowRectangles =
                for windowRectangles $ \(window, rectangle) ->
                -- SAFETY: The preferred dimensions for any window gets
                -- calculated when it first gets mapped, so it should exist.
                let preferredDimension = dimensions Map.! window
                    newRectangle = if Set.member window windows
                    then pseudoTileDimension preferredDimension rectangle
                    else rectangle
                in (window, newRectangle)
        in (newWindowRectangles, Nothing)

    handleMess :: PseudoTiling Window
               -> SomeMessage
               -> X (Maybe (PseudoTiling Window))
    handleMess pseudoTiler@(PseudoTiling dimensions windows) message
        | Just MapRequestEvent { ev_window = window } <- fromMessage message = do
            dimensionsForWindow <- getPreferredDimensions window
            let newDimensions = Map.insert window dimensionsForWindow dimensions
            return . Just $ pseudoTiler{ preferredDimensions = newDimensions }
        | Just (SetWindow window) <- fromMessage message =
            return . Just $ pseudoTiler{
                pseudoWindows = Set.insert window windows
            }
        | Just (ToggleWindow window) <- fromMessage message =
            return . Just $ pseudoTiler{
                pseudoWindows = toggleMember window windows
            }
        | Just DestroyWindowEvent { ev_window = window } <- fromMessage message =
            return . Just $ pseudoTiler{
                preferredDimensions = Map.delete window dimensions,
                pseudoWindows = Set.delete window windows
            }
      where
        toggleMember el set = if Set.member el set
            then Set.delete el set
            else Set.insert el set
    handleMess _ _ = return Nothing


-- Public API -----------------------------------------------------------------

pseudoTiling :: layout window -> LM.ModifiedLayout PseudoTiling layout window
pseudoTiling = LM.ModifiedLayout (PseudoTiling Map.empty Set.empty)


doPseudoTile :: ManageHook
doPseudoTile = do
    window <- ask
    liftX . broadcastMessage $ SetWindow window
    idHook


-- Helper functions -----------------------------------------------------------

getPreferredDimensions :: Window -> X Dimensions
getPreferredDimensions window = withDisplay $ \display -> do
    attributes <- liftIO $ getWindowAttributes display window
    let preferredWidth = wa_width attributes + 2 * wa_border_width attributes
    let preferredHeight = wa_height attributes + 2 * wa_border_width attributes
    return (fi preferredWidth, fi preferredHeight)


pseudoTileDimension :: Dimensions -> Rectangle -> Rectangle
pseudoTileDimension (wPreferred, hPreferred) rectangleBefore =
    let Rectangle xBefore yBefore wBefore hBefore = rectangleBefore
        -- don't extend beyond the underlying layout's allocated space
        w = min wPreferred wBefore
        h = min hPreferred hBefore
        -- center
        x = xBefore + fi (wBefore `div` 2) - fi (w `div` 2)
        y = yBefore + fi (hBefore `div` 2) - fi (h `div` 2)
    in Rectangle x y w h


for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

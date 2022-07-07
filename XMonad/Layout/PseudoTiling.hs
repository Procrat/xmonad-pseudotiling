{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StandaloneDeriving    #-}


module XMonad.Layout.PseudoTiling (
    pseudoTiling,
    doPseudoTile,
    PseudoTilingMessage(..),
    eventHook,
) where


import           Control.Monad                (forM)
import           Data.Monoid                  (All (..))
import           Data.Set                     (Set)
import qualified Data.Set                     as Set

import           XMonad                       hiding (display, windows)
import qualified XMonad.Layout.LayoutModifier as LM
import qualified XMonad.StackSet              as W
import           XMonad.Util.XUtils           (fi)


type Dimensions = (Dimension, Dimension)


newtype PseudoTiling window = PseudoTiling
    { pseudoWindows       :: Set window
    } deriving (Read)
deriving instance Show window => Show (PseudoTiling window)


data PseudoTilingMessage = SetWindow Window
                         | ToggleWindow Window
    deriving Typeable

instance Message PseudoTilingMessage


instance LM.LayoutModifier PseudoTiling Window where
    redoLayout :: PseudoTiling Window
                 -> Rectangle
                 -> Maybe (W.Stack Window)
                 -> [(Window, Rectangle)]
                 -> X ([(Window, Rectangle)], Maybe (PseudoTiling Window))
    redoLayout PseudoTiling { pseudoWindows } _ _ windowRectangles = do
        newWindowRectangles <- forM windowRectangles $ \(window, rectangle) -> do
            preferredDimensions <- getPreferredDimensions window
            let newRectangle = if Set.member window pseudoWindows
                then pseudoTileDimension preferredDimensions rectangle
                else rectangle
            return (window, newRectangle)
        return (newWindowRectangles, Nothing)

    handleMess :: PseudoTiling Window
               -> SomeMessage
               -> X (Maybe (PseudoTiling Window))
    handleMess pseudoTiler@PseudoTiling { pseudoWindows } message
        | Just (SetWindow window) <- fromMessage message =
            return . Just $ pseudoTiler {
                pseudoWindows = Set.insert window pseudoWindows
            }
        | Just (ToggleWindow window) <- fromMessage message =
            return . Just $ pseudoTiler {
                pseudoWindows = toggleMember window pseudoWindows
            }
        | Just DestroyWindowEvent { ev_window = window } <- fromMessage message =
            return . Just $ pseudoTiler {
                pseudoWindows = Set.delete window pseudoWindows
            }
      where
        toggleMember el set = if Set.member el set
            then Set.delete el set
            else Set.insert el set
    handleMess _ _ = return Nothing


-- Public API -----------------------------------------------------------------

pseudoTiling :: layout window -> LM.ModifiedLayout PseudoTiling layout window
pseudoTiling = LM.ModifiedLayout (PseudoTiling Set.empty)


eventHook :: Event -> X All
eventHook MapRequestEvent { ev_window = window } = do
    dimensions <- getInitialPreferredDimensions window
    setPreferredDimensions window dimensions
    return $ All True
eventHook _ = return $ All True


doPseudoTile :: ManageHook
doPseudoTile = do
    window <- ask
    liftX . broadcastMessage $ SetWindow window
    idHook


-- Helper functions -----------------------------------------------------------

getInitialPreferredDimensions :: Window -> X Dimensions
getInitialPreferredDimensions window = withDisplay $ \display -> do
    attributes <- liftIO $ getWindowAttributes display window
    let preferredWidth = wa_width attributes + 2 * wa_border_width attributes
    let preferredHeight = wa_height attributes + 2 * wa_border_width attributes
    return (fi preferredWidth, fi preferredHeight)


getPreferredDimensions :: Window -> X Dimensions
getPreferredDimensions window = withDisplay $ \display -> do
    prop <- getAtom preferredDimensionsPropName
    Just [width, height] <- io $ getWindowProperty32 display prop window
    return (fi width, fi height)


setPreferredDimensions :: Window -> Dimensions -> X ()
setPreferredDimensions window dimensions = withDisplay $ \display -> do
    prop <- getAtom preferredDimensionsPropName
    let (width, height) = dimensions
    io $ changeProperty32 display window prop cARDINAL propModeReplace [fi width, fi height]


preferredDimensionsPropName :: String
preferredDimensionsPropName = "xmonad_pseudotiling_preferred_dimensions"


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

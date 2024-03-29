# XMonad.Layout.PseudoTiling #

This is an [XMonad](http://xmonad.org) extension that **lets windows have their
preferred size**. If you ever noticed that windows were being unnaturally
stretched, this layout modifier will fix that.

The pseudotiling layout modifier will try to set the size of windows to those
specified by the application but bounded by the space that the underlying layout
would allocate to the window. If it is smaller, the window is centered in that
space.

![Screenshot with one window](screenshots/screenshot0.png)
![Screenshot with two windows](screenshots/screenshot1.png)
![Screenshot with three windows](screenshots/screenshot2.png)


## Usage

### Hold my hand

Clone this repository into your `~/.xmonad/lib` directory or make it available
to you in another way.
```sh
git clone git@github.com:Procrat/xmonad-pseudotiling ~/.xmonad/lib/
```

Add the following to the imports of your `~/.xmonad/xmonad.hs`.
```haskell
import qualified XMonad.Layout.PseudoTiling     as PseudoTiling
import           XMonad.Layout.PseudoTiling     (doPseudoTile, pseudoTiling)
```

`pseudoTiling` is the name of the layout modifier where the magic happens. Add
it to your layout hook:
```haskell
main = xmonad $ def {
    layoutHook = myLayouts
}

myLayouts = pseudoTiling $ tiled ||| Mirror tiled ||| Full
  where tiled = Tall 1 3/100 1/2
```

I personally like to use this in combination with the fantastic
[XMonad.Layout.LayoutHints](https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Layout-LayoutHints.html)
module:
```haskell
myLayouts = modifiers layouts
  where
    modifiers = layoutHintsWithPlacement (0.5, 0.5) . pseudoTiling
    layouts = tiled ||| Mirror tiled ||| Full
    tiled = Tall 1 3/100 1/2
```

To be able to know what size to pseudotile windows to, we also need to hook into
some X events, so add the pseudotiling event hook as well:
```haskell
main = xmonad $ def {
    layoutHook      = myLayouts
    handleEventHook = PseudoTiling.eventHook <+> handleEventHook def
}
```

This by itself won't have any effect. It allows you to pseudotile windows, but
it doesn't decide on **when** to do that. You have two options, which you can
perfectly combine as well.

#### Pseudotile new windows

To have new windows be pseudotiled automatically, add `doPseudoTile` to your
manage hook:
```haskell
main = xmonad $ def {
    layoutHook      = myLayouts
    handleEventHook = PseudoTiling.eventHook <+> handleEventHook def
    manageHook      = myManageHook <+> manageHook def
}

myManageHook :: ManageHook
myManageHook = doPseudoTile
```

You can also choose to have only certain applications pseudotiled, e.g. URxvt
```haskell
myManageHook :: ManageHook
myManageHook = className =? "urxvt" --> doPseudoTile
```


#### Pseudotile on demand

To have windows only pseudotile when you explicitly ask it to, send the message
`SetWindow` or `ToggleWindow` with a given window.

For example, to make the binding `Mod+Shift+P` toggle the pseudotiling status of
the window in focus (using
[EZConfig](http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Util-EZConfig.html)-style
bindings):
```haskell
    ...
    , ("M-S-p", withFocused $ sendMessage . PseudoTiling.ToggleWindow)
    ...
```


### Just give me the API

I think the combinations of function names and types are pretty
self-explanatory. If they are not, feel free to tell me so.
```haskell
pseudoTiling :: layout a -> ModifiedLayout PseudoTiling layout a
eventHook :: Event -> X All
doPseudoTile :: ManageHook
data PseudoTilingMessage = SetWindow Window | ToggleWindow Window
```


## Acknowledgements

The idea of pseudotiling windows isn't mine but from the very fine window
manager [herbstluftwm](https://www.herbstluftwm.org/). Much appreciation goes
out to the people who came up with this idea! :heart:

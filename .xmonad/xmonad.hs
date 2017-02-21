{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}
import Data.Monoid
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Reflect
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Reflect
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.OneBig
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Util.Cursor
import XMonad.Util.Dmenu
import XMonad.Util.EZConfig
import System.Taffybar.Hooks.PagerHints (pagerHints)
import System.Exit
import Control.Monad

import XMonad.Layout.Circle
import XMonad.Layout.DecorationMadness
import XMonad.Actions.WindowMenu
import XMonad.Layout.ImageButtonDecoration
import XMonad.Layout.Minimize
import XMonad.Layout.Maximize

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
    transform TABBED x k = k (tabbedBottom shrinkText myTabTheme) (const x)

myTabTheme = defaultTheme { inactiveBorderColor = "#E9EFDA"
                           , inactiveTextColor   = "#E9EFDA"
                           , inactiveColor       = "#4B5853"
                           , activeColor         = "#507568"
                           , activeTextColor     = "#F7FFBC"
                           , activeBorderColor   = "#F7FFBC"
                           , fontName            = "xft:Exo 2-10"
                           }

myTheme :: Theme
myTheme =
    Theme { activeColor         = "#57946B"
          , activeBorderColor   = "#507568"
          , activeTextColor     = "#1B2423"
          , inactiveColor       = "#1B2423"
          , inactiveBorderColor = "#4B5853"
          , inactiveTextColor   = "#E9EFDA"
          , urgentColor         = "#FFFF00"
          , urgentBorderColor   = "#00FF00"
          , urgentTextColor     = "#FF0000"
          , fontName            = "xft:Exo 2-10"
          , decoWidth           = 1200
          , decoHeight          = 19
          , windowTitleAddons   = []
          , windowTitleIcons    = []
          }

quitWithWarning :: X ()
quitWithWarning = do
    let m = "confirm quit"
    s <- dmenu [m]
    when (m == s) (io exitSuccess)


myLayout = lessBorders Screen
    $ mkToggle (single TABBED)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ mkToggle (single MIRROR)
    $ Tall 1 (3/100) (1/2) ||| OneBig 0.75 0.65 ||| spiral (6/7) ||| MosaicAlt M.empty

myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer" --> doFloat
    , className =? "scosk" --> doFloat ]

modm = mod4Mask

laptopConfig config = id $ config
    { borderWidth = 2
    , focusedBorderColor = "#f12c35"
    }

myTerminal = "termite"
myAltTerminal = "xterm"
myTertTerminal = "urxvtc"

main = xmonad $ ewmh $ pagerHints $ defaultConfig
    { startupHook        = setDefaultCursor xC_left_ptr
    , modMask            = mod4Mask
    , terminal           = myTerminal
    , handleEventHook    = fullscreenEventHook
    , layoutHook         = {- mkToggle (single NBFULL) $ -} avoidStruts myLayout
    , manageHook         = myManageHook <+> manageDocks
    , normalBorderColor  = "#2a3533"
    , focusedBorderColor = "#c26157"
    -- , logHook         = updatePointer (Relative 0.5 0.5)
    }
    `additionalKeysP`
    [ ("M-S-a", windows W.swapMaster )
    , ("M-a", windows W.focusMaster )
    , ("M-S-q", quitWithWarning )
    , ("M-S-C-q", io exitSuccess )
    , ("M-c", kill )
    , ("M-<Return>", spawn myTerminal )
    , ("M-S-<Return>", spawn myAltTerminal )
    , ("M-C-<Return>", spawn myTertTerminal )
    , ("M-S-C-<Return>", spawn "st" )
    , ("M-d", spawn "xboomx" )
    , ("M-S-d", spawn "rofi -show run" )
    , ("M-S-p", spawn "rofi -show drun" )
    , ("M-w", spawn "rofi -show window" )
    , ("M-v", spawn "pavucontrol" )
    , ("M-g", spawn "gcolor2" )
    , ("M-m", spawn $ myTertTerminal ++ " -e ranger" )
    , ("M-S-m", spawn "pcmanfm" )
    , ("M-`", spawn "mousepad" )
    , ("M-S-`", spawn $ myTerminal ++ " -e nvim" )
    , ("M-S-h", spawn $ myAltTerminal ++ " -e htop" )
    , ("M-C-j", spawn $ myAltTerminal ++ " -e julia" )
    , ("M-C-p", spawn $ "pamac-manager" )
    , ("M-n", refresh )
    , ("<Print>", spawn "scrot" )
    , ("<XF86AudioMute>", spawn "pamixer -t" )
    , ("<XF86AudioLowerVolume>", spawn "pamixer -d 12" )
    , ("<XF86AudioRaiseVolume>", spawn "pamixer -i 12" )
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 12" )
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 12" )
    , ("M-b", sendMessage $ ToggleStruts )
    , ("M-f", sendMessage $ Toggle TABBED )
    --, ("M-S-f", sendMessage $ Toggle NBFULL )
    , ("M-x", sendMessage $ Toggle REFLECTX )
    , ("M-y", sendMessage $ Toggle REFLECTY )
    , ("M-z", sendMessage $ Toggle MIRROR )
    , ("M-s", nextScreen )
    , ("M-S-s", shiftNextScreen >> nextScreen )
    , ("M-;", swapNextScreen )
    , ("M-<L>", moveTo Prev NonEmptyWS )
    , ("M-<R>", moveTo Next NonEmptyWS )
    , ("M-S-<L>", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS )
    , ("M-S-<R>", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS )
    , ("M-o", windowMenu )
    ]
    `additionalMouseBindings`
    [ ((modm, button4), const prevWS )
    , ((modm, button5), const nextWS )
    , ((modm .|. shiftMask, button4), \_ -> shiftTo Prev AnyWS >> prevWS)
    , ((modm .|. shiftMask, button5), \_ -> shiftTo Next AnyWS >> nextWS)
--    , ((modm .|. shiftMask .|. controlMask, button4),  windows W.focusDown )
    , ((modm .|. controlMask, button4), \_ -> sendMessage Expand )
    , ((modm .|. controlMask, button5), \_ -> sendMessage Shrink )
    ]

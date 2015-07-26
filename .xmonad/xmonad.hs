{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, TypeSynonymInstances, MultiParamTypeClasses #-}
import Data.Monoid
import qualified Data.Map        as M
import qualified XMonad.StackSet as W
import XMonad
import XMonad.Config.Desktop
import XMonad.Actions.CycleWS
import XMonad.Hooks.EwmhDesktops 
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
import XMonad.Util.EZConfig 
import System.Taffybar.Hooks.PagerHints (pagerHints)

data TABBED = TABBED deriving (Read, Show, Eq, Typeable)
instance Transformer TABBED Window where
        transform TABBED x k = k (tabbedBottom shrinkText defaultTheme) (const x)

myTerminal = "urxvtc"

myLayout = lessBorders Screen
    $ mkToggle (single TABBED)
    $ mkToggle (single REFLECTX)
    $ mkToggle (single REFLECTY)
    $ mkToggle (single MIRROR)
    $ Tall 1 (3/100) (1/2) ||| OneBig 0.75 0.65 ||| spiral (6/7) ||| MosaicAlt M.empty

modm = mod4Mask

main = xmonad $ ewmh $ pagerHints $ desktopConfig
	{ modMask = mod4Mask 
	, terminal = myTerminal
        , handleEventHook    = fullscreenEventHook
	, layoutHook = {- mkToggle (single NBFULL) $ -} desktopLayoutModifiers myLayout
        , normalBorderColor  = "#c6cfbf"
        , focusedBorderColor = "#c26157"
	}
	`additionalKeysP`
	[ ("M-S-a", windows W.swapMaster )
	, ("M-a", windows W.focusMaster ) 
	, ("M-c", kill ) 
	, ("M-S-<Return>", spawn myTerminal )
	, ("M-<Return>", spawn "st" )
	, ("M-d", spawn "xboomx" )
	, ("M-v", spawn "pavucontrol" )
	, ("M-m", spawn "st -e ranger" )
	, ("M-S-m", spawn "thunar" )
	, ("M-n", refresh )
	, ("M-f", sendMessage $ Toggle TABBED )
	--, ("M-S-f", sendMessage $ Toggle NBFULL )
	, ("M-x", sendMessage $ Toggle REFLECTX )
	, ("M-y", sendMessage $ Toggle REFLECTY )
	, ("M-z", sendMessage $ Toggle MIRROR )
	, ("M-<L>", moveTo Prev NonEmptyWS )
	, ("M-<R>", moveTo Next NonEmptyWS )
	, ("M-S-<L>", shiftTo Prev NonEmptyWS >> moveTo Prev NonEmptyWS )
	, ("M-S-<R>", shiftTo Next NonEmptyWS >> moveTo Next NonEmptyWS )
	]
	`additionalMouseBindings`
	[ ((modm, button4), const prevWS )
	, ((modm, button5), const nextWS )
	, ((modm .|. shiftMask, button4), \_ -> shiftTo Prev AnyWS >> prevWS)
	, ((modm .|. shiftMask, button5), \_ -> shiftTo Next AnyWS >> nextWS)
	, ((modm .|. controlMask, button4), \_ -> sendMessage Expand )
	, ((modm .|. controlMask, button5), \_ -> sendMessage Shrink )
	]

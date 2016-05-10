import System.Taffybar

import System.Taffybar.Systray
import System.Taffybar.TaffyPager
import System.Taffybar.SimpleClock
import System.Taffybar.FreedesktopNotifications
import System.Taffybar.Weather
import System.Taffybar.MPRIS2
import System.Taffybar.Battery

import System.Taffybar.Widgets.PollingBar
import System.Taffybar.Widgets.PollingGraph

import System.Information.Memory
import System.Information.CPU
import System.Information.Battery

import Graphics.UI.Gtk.General.RcStyle -- (rcParseString)
import System.Environment.XDG.BaseDir --( getUserConfigFile )
--import Color (Color(..), hexColor)

import System.Taffybar.Pager(colorize,escape)

cf00 = "#E9AC9C"
df00 = "#C27157"
cff0 = "#F6C384"
dff0 = "#B09440"
c0f0 = "#BAC172"
d0f0 = "#5E944C"
c0ff = "#99DAA2"
d0ff = "#1CA790"
c00f = "#86C6B9"
d00f = "#548CA1"
cf0f = "#CFC8CB"
df0f = "#BB8699" 

memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (userLoad, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

batCfg :: BarConfig
batCfg =
  (defaultBarConfig colorFunc) { barPadding = 1
  , barBackgroundColor = const (0.243137, 0.290196, 0.278431)
  , barBorderColor = (0.105882, 0.141176, 0.137255)
  } where
    colorFunc pct
      | pct < 0.12 = (1.0, 0.0, 0.0) -- alert
      | pct > 0.12 && pct < 0.4 = (0.760784, 0.443137, 0.341176) -- red
      | pct > 0.4 && pct < 0.6 = (0.730784, 0.513137, 0.301176) -- trumpskin
      | pct > 0.6 && pct < 0.8 = (0.690196, 0.580392, 0.2509) -- yellow
      | pct > 0.8 = (0.368627, 0.580392, 0.298039) -- green
      | otherwise = (0, 1, 0) -- grey

main = do
  let cfg = defaultTaffybarConfig { barHeight = 16
                                  , widgetSpacing = 10
                                  }
--      font = "Lato 12"
--      fgColor = hexColor $ RGB (0.51, 0.58, 0.59)
--      bgColor = hexColor $ RGB (0.0, 0.17, 0.21)
--      textColor = hexColor $ RGB (0.58, 0.63, 0.63)

  let memCfg = defaultGraphConfig { graphDataColors = [(0.760784, 0.443137, 0.341176, 1)]
                                  , graphLabel = Just "<span foreground='#66746E'>mem</span>"
                                  , graphBorderWidth = 0
                                  , graphBackgroundColor = (0.105882, 0.141176, 0.137255)
                                  }
      cpuCfg = defaultGraphConfig { graphDataColors = [ (0.482353, 0.729412, 0.372549, 1)
                                                      , (0.807843, 0.6, 0.835294, 0.5)
                                                      ]
                                  , graphLabel = Just "<span foreground='#66746E'>cpu</span>"
                                  , graphBorderWidth = 0
                                  , graphBackgroundColor = (0.105882, 0.141176, 0.137255)
                                  }
  let clock = textClockNew Nothing ("<span fgcolor='"++dff0++"'>%a %b %_d %H:%M</span>") 1
      pager = taffyPagerNew defaultPagerConfig
        { emptyWorkspace = colorize "#66746E" "" . escape 
        , activeWorkspace = colorize cff0 "" . (\s -> "[" ++ s ++ "]" ) . escape
        }
--      log = xmonadLogNew
      note = notifyAreaNew defaultNotificationConfig
      wea = weatherNew ((defaultWeatherConfig "KIAD") { weatherTemplate = "$tempF$ °F $humidity$% $pressure$ hPa $skyCondition$"} ) 10
      mpris = mpris2New
      mem = pollingGraphNew memCfg 1 memCallback
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      tray = systrayNew
      bat = batteryBarNew (batCfg {barPadding = 0}) 1 -- batteryBarNew defaultBatteryConfig 1

  rcParseString $ ""
        ++ "style \"default\" {"
--        ++ " font_name = \"" ++ font ++ "\""
--        ++ " bg[NORMAL] = \"" ++ solarizedBase03 ++ "\""
--        ++ " fg[NORMAL] = \"" ++ solarizedBase3 ++ "\""
--        ++ " text[NORMAL] = \"" ++ solarizedBlue ++ "\""
        ++ "}"

  defaultTaffybar cfg { startWidgets = [ pager, note ] --log
                                        , endWidgets = [ tray, wea, clock, bat, mem, cpu, mpris ]
                                        }


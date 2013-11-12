import XMonad
import Data.List -- for `isSuffixOf`
import qualified Data.Map as M
import XMonad.Config.Desktop
import XMonad.Config.Gnome

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.ComboP
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Maximize
import XMonad.Layout.NoBorders
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import Data.Ratio ((%)) -- for IM

import XMonad.Actions.FindEmptyWorkspace
import XMonad.Actions.WindowGo

import XMonad.Util.NamedScratchpad
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig
import qualified XMonad.StackSet as W

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)

import Control.Monad

import System.IO


{- General config -}

myTerminal              = "gnome-terminal"
myFileManager           = "nautilus --no-desktop ~"
myModP                  = "dmenu-launch"
modm                    = mod4Mask -- Win key for Mod

myLogout                = "gnome-session-quit --no-prompt"
myLock                  = "gnome-screensaver-command -l"
myGnomeControlCenter    = "gnome-control-center"

myGimpWorkspace         = "7"
myImWorkspace           = "9"

myBorderWidth		= 2
myFocusFollowsMouse 	= False
myInactiveBorderColor 	= "#333333"
myActiveBorderColor 	= "#00AA00"

myMusicPlayPause :: X ()
myMusicPlayPause        = spawn "~/apps/scripts/mpd-playpause.sh"

{- custom *Config for easy swapping -}
myConfig                = defaultConfig
--myConfig                = gnomeConfig


{- Keys -}

-- workspace keys
myWorkspaces = map show [1..10]
myWorkspaceKeysP = [ "<F" ++ n ++ ">" | n <- myWorkspaces ]

myKeysP =
  [ -- Mod-(shift)-F* for switching workspaces and moving windows between them
    ("M-" ++ mask ++ key, windows $ fn ws)
      | (ws, key)  <- zip myWorkspaces myWorkspaceKeysP
      , (fn, mask) <- [(W.greedyView, ""), (W.shift, "S-")]
  ]
  ++
  [ ("M-t", spawn myTerminal)
  , ("M-f", spawn myFileManager)
  , ("M-p", spawn myModP)
  , ("M-C-<Escape>", kill) -- close window

--  , ("M-S-q", spawn myLogout)
  , ("M-S-l", spawn myLock)
  , ("M-S-o", spawn myGnomeControlCenter)

  , ("M-S-t", withFocused $ windows . W.sink) -- unfloat
  , ("M-\\",  withFocused $ sendMessage . maximizeRestore)
  , ("M-m",   viewEmptyWorkspace)

  , ("M-u", sendMessage Taller)
  , ("M-i", sendMessage Wider)
  , ("M-r", sendMessage Reset)

  , ("M-C-s", namedScratchpadAction myScratchpads "stalonetray")
  , ("M-C-n", namedScratchpadAction myScratchpads "nm-configuration-editor")
  ]

myRemoveKeysP = 
  [ "M-S-c" {- default kill -}
  ]


{- Window rules -}

myBaseManageHook = namedScratchpadManageHook myScratchpads <+> manageDocks <+> manageHook myConfig

myManageHook = composeAll
  [ myBaseManageHook
  , className =? "Google-chrome" <||> className =? "Chromium" --> doShift "1"
  , className =? "Chromium" <&&> title =? "Chromium Preferences" --> doFloat
  , className =? "Eclipse" --> doShift "3"
  , className =? "Firefox" <||> className =? "Namoroka" --> doShift "1"
  , className =? "Gitg" --> doShift "4"
  , className =? "psi" --> doShift "9"
  , className =? "Skype" --> doShift "9"
  , className =? "Sonata" --> doShift "9"
  , className =? "Wine" <&&> title =? "English-Czech - Lexicon" --> doShift "2"
  , className =? "Xmessage" --> doFloat
  , className =? "Gimp-2.8" --> doShift myGimpWorkspace
  , className =? "Gimp-2.8" <&&> windowRole =? "gimp-toolbox-color-dialog" --> doFloat
  , className =? "file_properties" --> doFloat
  , className =? "Gnome-system-monitor" --> doFloat
  , className =? "Gnome-display-properties" --> doFloat
  , className =? "Remmina" --> doShift "10"
  , className =? "Do"  --> doIgnore -- Gnome Do
  , className =? "Gcalctool" --> doFloat
  , isFullscreen --> doFullFloat -- needs: import XMonad.Hooks.ManageHelpers (isFullscreen,doFullFloat)
  ]

windowRole = stringProperty "WM_WINDOW_ROLE"

{-- Scratchpads --}

myScratchpads =        
        [ NS "stalonetray" "stalonetray" (className =? "stalonetray" ) defaultFloating
        , NS "nm-connection-editor" "nm-connection-editor" (className =? "Nm-connection-editor" ) defaultFloating
        , NS "XMind" "XMind" (className =? "XMind" ) defaultFloating
        , NS "evolution" "evolution" (className =? "Evolution" ) defaultFloating
        , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol" ) defaultFloating
        , NS "stardict" "stardict" (className =? "Stardict")
                (customFloating $ W.RationalRect (2/5) (2/5) (1/2) (1/2))
        ]


{- Layouts -}

myLayoutHook = avoidStruts $ desktopLayoutModifiers $ smartBorders $ maximize
  $ onWorkspace myGimpWorkspace (myDefaultLayouts ||| myGimpLayout)
  $ onWorkspace myImWorkspace (myImLayout ||| myDefaultLayouts)
  $ myDefaultLayouts

myDefaultLayouts = layoutHook myConfig
		 ||| simpleTabbed
                 ||| GridRatio (2/3)
                 ||| mosaic 1.5 [3, 2]

myGimpLayout = withIM (0.15) (Role "gimp-toolbox")
             $ reflectHoriz
             $ withIM (0.15) (Role "gimp-dock") simpleTabbed

myImLayout = reflectHoriz $ withIM (1%7) (And (ClassName "psi") (Resource "main"))
           $ reflectHoriz $ GridRatio (1/2)


{- Everything glued together -}

main :: IO ()
main = do
  xmproc <- spawnPipe "/usr/bin/xmobar ~/.xmonad/xmobarrc"
  xmonad $ withUrgencyHook NoUrgencyHook $ myConfig
    -- General configuration. Mostly uses custom my* functions defined earlier.
    { manageHook         = myManageHook
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , terminal           = myTerminal
    , modMask            = modm
    , borderWidth        = myBorderWidth
    , focusFollowsMouse	 = myFocusFollowsMouse
    , normalBorderColor  = myInactiveBorderColor
    , focusedBorderColor = myActiveBorderColor
    , logHook            = dynamicLogWithPP $ xmobarPP
                           { ppOutput = hPutStrLn xmproc
                           , ppTitle = xmobarColor "green" "" . shorten 100
                           }
    } 
    `additionalKeysP` myKeysP
    `removeKeysP` myRemoveKeysP
 

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Config.Gnome
import XMonad.Config.Desktop
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Layout.Named
import XMonad.Layout.ShowWName
import XMonad.Layout.NoBorders
import XMonad.Layout.SimpleDecoration
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.Grid
import XMonad.Layout.Mosaic
import XMonad.Layout.FixedColumn
import XMonad.Layout.Magnifier
import XMonad.Layout.IM
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.Dishes
import XMonad.Layout.Fullscreen
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ThreeColumns
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.LimitWindows
import XMonad.Layout.NoBorders
import XMonad.Util.Themes
import XMonad.Util.Run(spawnPipe)
import System.IO
import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Actions.RotSlaves
import XMonad.Actions.PerWorkspaceKeys
import qualified XMonad.Actions.FlexibleResize as Flex
import XMonad.Util.Scratchpad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Control.Monad
import Data.Ratio ((%))
import Data.Maybe
import Data.List

-- Windows (apple) key
myModMask = mod4Mask

colorCoral   = "#C34A2C"
colorGrey    = "#cccccc"
colorBlack  = "#020202"

myWorkspaces = ["main", "web", "code", "4", "5", "6", "7", "8", "9"]

myTile = named "Tall" $ ResizableTall 1 (3/100) (1/2) []
myCode = named "Coding" $ magnifiercz' 1.2 $ FixedColumn 1 1 80 10
myWide = named "On Top" $ Mirror myTile
myWideFlip = named "On Bottom" $ reflectVert $ myWide
myAllWindows = named "All Windows" $ noBorders (fullscreenFull Grid)
noTitles l = desktopLayoutModifiers l

myLayouts = smartBorders $ noTitles 
        $ onWorkspace "code" codeLayouts
        $ allLayouts
    where
        allLayouts = myTile ||| myWide ||| myWideFlip ||| Full
        codeLayouts = myCode ||| myTile ||| myWide ||| myWideFlip ||| Full

myLayoutHook = toggleLayouts myAllWindows myLayouts
    
-- myDmenu = "dmenu_recent -b -i -p Run -nf black -sf white -sb steelblue -nb lightsalmon"
myDmenu = "dmenu_recent -p Run -i"

myKeys =
    [ ("M-g", sendMessage Shrink)
    , ("M-S-z", spawn "gnome-session-quit")
    , ("M-S-\\", spawn "google-chrome")
    , ("M-S-a", sendMessage ToggleLayout)
    , ("M-S-;", spawn myDmenu)
    ]


myManageHook = composeAll (
	[ manageHook gnomeConfig
	, className =? "Unity-2d-panel" --> doIgnore
	, isFullscreen --> doFullFloat
	])

main = do
    xmproc <- spawnPipe "xmobar /home/matthew/.xmonad/.xmobarrc"
    xmonad $ gnomeConfig 
        { manageHook = myManageHook 
        , modMask = myModMask
        , workspaces = myWorkspaces
        , layoutHook = myLayoutHook
        , logHook = dynamicLogWithPP xmobarPP
                    { ppOutput = hPutStrLn xmproc
                    , ppTitle = xmobarColor "#ff9696" "" . shorten 50
                    }
        , normalBorderColor = colorBlack
        , focusedBorderColor = colorCoral
        } 
        `additionalKeysP` myKeys

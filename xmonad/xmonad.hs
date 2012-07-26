import XMonad
import XMonad.Util.Run --(spawnPipe)
import XMonad.Util.EZConfig --(additionalKeys)
import System.IO
import XMonad.Config.Gnome

import XMonad.Actions.CycleWS

-- Import for hooks
import XMonad.Operations
import System.Exit
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

-- Import for layout
import XMonad.Layout.NoBorders (smartBorders, noBorders)
import XMonad.Layout.Fullscreen 
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.IM
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutHints
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts

import Data.Ratio ((%))

import Data.String.UTF8
 
import qualified XMonad.StackSet as W
import qualified Data.Map as M

-- Start config

-- Dzen/conky for bars
myXmonadBar = "dzen2 -x '0' -y '0' -h '24' -w '920' -ta '1' -fg '#FFFFFF' -bg '#1B1D1E' -fn 'Bitstream Vera Mono-10'"
myStatusBar = "conky -c /home/ross/.xmonad/.conky_dzen | dzen2 -x '770' -w '1000' -h '24' -ta 'r' -bg '#1B1D1E' -fg '#FFFFFF' -y '0' -fn 'Bitstream Vera Mono-10'"
myBitmapsDir = "/home/ross/.xmonad/dzen2"

--Workspace names
myWorkspaces :: [WorkspaceId]
myWorkspaces = clickable . (map dzenEscape) $ ["1:main", "2:web", "3:vim", "4:chat", "5:music", "6:other", "7:shed", "8:theatre", "9:cinema"]
  where clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = i ]

-- Set workspace names
mainWs      = (myWorkspaces !! 0)
webWs      = (myWorkspaces !! 1)
vimWs       = (myWorkspaces !! 2)
chatWs     = (myWorkspaces !! 3)
musicWs     = (myWorkspaces !! 4)
otherWs     = (myWorkspaces !! 5)
shedWs      = (myWorkspaces !! 6)
theatreWs   = (myWorkspaces !! 7)
cinemaWs    = (myWorkspaces !! 8)

-- Set border colours
myFocusedBorderColor = "#33cc33"
myNormalBorderColor = "m#00cd00"

myBorderWidth     = 2

main = do
    dzenLeftBar <- spawnPipe myXmonadBar
    dzenRightBar <- spawnPipe myStatusBar
    xmonad $ defaultConfig
        { manageHook = manageHook' 
--        , layoutHook = avoidStruts $ layoutHook defaultConfig
        , layoutHook = layoutHook'
        , logHook = myLogHook dzenLeftBar >> fadeInactiveLogHook 0xdddddddd
--        , logHook = dynamicLogWithPP xmobarPP
--                        { ppOutput = hPutStrLn xmproc
--                        , ppTitle = xmobarColor "green" "" . shorten 50
--                        }
        , workspaces  = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth        = myBorderWidth
        , modMask = mod4Mask -- Bind mod to win key
        } `additionalKeys`
        [ ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
        , ((controlMask, xK_Print), spawn "sleep 0.2; scrot -s")
        , ((0, xK_Print), spawn "scrot")
        , ((0                   , 0x1008FF11), spawn "amixer set Master 2-")
        , ((0                   , 0x1008FF13), spawn "amixer set Master 2+")
        , ((mod4Mask            , xK_Down),    swapNextScreen)
        , ((mod4Mask            , xK_f),        sendMessage ToggleLayout)
        ]


-- Hooks {{{
-- ManageHook {{{
manageHook' :: ManageHook
manageHook' = (composeAll . concat $
    [ [resource     =? r            --> doIgnore            |   r   <- myIgnores] -- ignore desktop
    , [className    =? d            --> doShift  mainWs     |   d   <- myDev    ] -- move dev to main
    , [className    =? w            --> doShift  webWs      |   w   <- myWebs   ] -- move webs to main
    , [className    =? v            --> doShift  vimWs      |   v   <- myVim    ] -- move webs to main
    , [className    =? c            --> doShift	 chatWs     |   c   <- myChat   ] -- move chat to chat
    , [className    =? m            --> doShift  musicWs    |   m   <- myMusic  ] -- move music to music
    , [className    =? o            --> doShift  otherWs    |   o   <- myOther  ] -- move img to div
    , [className    =? s            --> doShift  shedWs     |   s   <- myShed   ] -- move img to div
    , [className    =? t            --> doShift  theatreWs  |   t   <- myTheatre] -- move img to div
    , [className    =? p            --> doShift  cinemaWs   |   p   <- myCinema ] -- move img to div
    , [className    =? f            --> doCenterFloat       |   f   <- myFloats ] -- float my floats
    , [name         =? n            --> doCenterFloat       |   n   <- myNames  ] -- float my names
    , [isFullscreen                 --> myDoFullFloat                           ]
    , [ manageDocks ]
    ]) 
 
  where

        role      = stringProperty "WM_WINDOW_ROLE"
        name      = stringProperty "WM_NAME"
 
        -- classnames
        myFloats  = ["Smplayer","MPlayer","VirtualBox","Xmessage","XFontSel","Downloads","Nm-connection-editor"]
        myWebs    = ["Firefox","Google-chrome","Chromium", "Chromium-browser"]
        myTheatre = ["Boxee","Trine"]
        myCinema  = ["Vlc"]
        myMusic	  = ["Rhythmbox","Spotify"]
        myChat	  = ["Pidgin","Buddy List"]
        myOther	  = ["Gimp"]
        myShed    = []
        myDev	  = ["gnome-terminal"]
        myVim	  = ["gvim"] -- Spelt wrong on purpose
 
        -- resources
        myIgnores = ["desktop","desktop_window","notify-osd","stalonetray","trayer"]
 
        -- names
        myNames   = ["bashrun","Google Chrome Options","Chromium Options"]
 
-- a trick for fullscreen but stil allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat
-- }}}

-- Layout
customLayout = smartBorders $ avoidStruts $ toggleLayouts Full  $ avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []
 
customLayout2 = smartBorders $ avoidStruts $ toggleLayouts Full  $ avoidStruts $ tiled ||| Mirror tiled ||| Full ||| simpleFloat 
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []
 
theatreLayout = fullscreenFull $ tiled ||| Mirror tiled ||| Full ||| simpleFloat 
  where
    tiled   = ResizableTall 1 (2/100) (1/2) []
 
cinemaLayout = noBorders $ Full ||| simpleFloat 

gimpLayout  = avoidStruts $ withIM (0.11) (Role "gimp-toolbox") $
              reflectHoriz $
              withIM (0.15) (Role "gimp-dock") Full
 
imLayout    = avoidStruts $ withIM (1%5) (And (ClassName "Pidgin") (Role "buddy_list")) Grid 
 --}}}

layoutHook'  =  id
                $ onWorkspaces [mainWs,musicWs] customLayout
                $ onWorkspace otherWs gimpLayout  
                $ onWorkspace chatWs imLayout 
                $ onWorkspace theatreWs theatreLayout 
                $ onWorkspace cinemaWs cinemaLayout 
                $ customLayout2

--Bar
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ defaultPP
    {
        ppCurrent           =   dzenColor "#a40e0e" "#5B5D5E" . pad . wrap "[ " " ]"
    --  , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
--      , ppVisible           =   dzenColor "#ebac54" "#4B4D4E" . pad 
      , ppVisible           =   dzenColor "#1212fc" "#5B5D5E" . pad 
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#ff0000" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "ResizableTall"             ->      "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
                                    "Mirror ResizableTall"      ->      "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
                                    "Full"                      ->      "^i(" ++ myBitmapsDir ++ "/full.xbm)"
                                    "Simple Float"              ->      "~"
                                    _                           ->      x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }
 


 
 

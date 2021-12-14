

{-# OPTIONS_GHC -W -fwarn-unused-imports -fno-warn-missing-signatures #-} 
import XMonad
import XMonad.Actions.Search
import XMonad.Actions.GridSelect
import System.Exit
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.NoBorders
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.Shell
import XMonad.Util.EZConfig(additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import Data.Char
import Data.Tree
import qualified Data.Map        as M
import qualified XMonad.Actions.Search as S
import qualified XMonad.Actions.TreeSelect as TS
import qualified XMonad.StackSet as W

calcPrompt :: XPConfig -> String -> X ()
calcPrompt c ans =
    inputPrompt c (trim ans) ?+ \input ->
        liftIO(runProcessWithInput "qalc" [input] "") >>= calcPrompt c
    where
        trim  = f . f
            where f = reverse . dropWhile isSpace
            
myFont :: String
myFont = "xft:Mononoki Nerd Font:bold:size=9:antialias=true:hinting=true"

myGSConfig = def
            { gs_cellheight  = 50
            , gs_cellwidth   = 250
            , gs_cellpadding = 10
            , gs_font        = myFont
            }
            
myGSConfig1 = def
            { gs_cellheight  = 50
            , gs_cellwidth   = 250
            , gs_cellpadding = 10
            , gs_font        = myFont
            }
            
myXPConfig :: XPConfig
myXPConfig = greenXPConfig { font = "xft:Profont:pixelsize=15:autohint=true",position = Top }

archwiki :: S.SearchEngine
archwiki = S.searchEngine "archwiki" "https://wiki.archlinux.org/index.php?search="
yahoo = S.searchEngine "yahoo" "https://yahoo.com/search="            

treeselectAction :: TS.TSConfig (X ()) -> X ()
treeselectAction a = TS.treeselectAction a
    [ Node (TS.TSNode "Shutdown" "Poweroff the system"      (spawn "shutdown")) []
       , Node (TS.TSNode "Recompile" "Recompile XMonad"     (spawn "xmonad --recompile")) []
       , Node (TS.TSNode "Restart" "Restart XMonad"         (spawn "xmonad --restart")) []
       , Node (TS.TSNode "Quit" "Restart XMonad"            (io exitSuccess)) []
       , Node (TS.TSNode "htop"     "a much better top"     (spawn (myTerminal ++ " -e htop"))) [] 
       , Node (TS.TSNode "glances"  "an eye on yout system" (spawn (myTerminal ++ " -e glances"))) []
       , Node (TS.TSNode "gtop"     "a more graphical top"  (spawn (myTerminal ++ " -e gtop"))) [] 
       , Node (TS.TSNode "nmon"     "network monitor"       (spawn (myTerminal ++ " -e nmon"))) [] 
       , Node (TS.TSNode "edit xmonad" "edit xmonad"        (spawn (myTerminal ++ " -e vim ~/.xmonad/xmonad.hs"))) []
    ]    
   
myTreeNavigation = M.fromList    
    [ ((0, xK_Escape),   TS.cancel)
    , ((0, xK_Return),   TS.select)
    , ((0, xK_space),    TS.select)
    , ((0, xK_Up),       TS.movePrev)
    , ((0, xK_Down),     TS.moveNext)
    , ((0, xK_Left),     TS.moveParent)
    , ((0, xK_Right),    TS.moveChild)
    , ((0, xK_k),        TS.movePrev)
    , ((0, xK_j),        TS.moveNext)
    , ((0, xK_h),        TS.moveParent)
    , ((0, xK_l),        TS.moveChild)
    , ((0, xK_o),        TS.moveHistBack)
    ]

-- Configuration options for treeSelect
tsDefaultConfig :: TS.TSConfig a
tsDefaultConfig = TS.TSConfig { TS.ts_hidechildren = True
                              , TS.ts_background   = 0xdd292d3e
                              , TS.ts_font         = myFont
                              , TS.ts_node         = (0xffd0d0d0, 0xff202331)
                              , TS.ts_nodealt      = (0xffd0d0d0, 0xff292d3e)
                              , TS.ts_highlight    = (0xffffffff, 0xff755999)
                              , TS.ts_extra        = 0xffd0d0d0
                              , TS.ts_node_width   = 200
                              , TS.ts_node_height  = 20
                              , TS.ts_originX      = 0
                              , TS.ts_originY      = 0
                              , TS.ts_indent       = 80
                              , TS.ts_navigate     = myTreeNavigation
                              }

myTerminal      = "alacritty"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myBorderWidth   = 3

myModMask       = mod4Mask

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

myWorkspaces :: [String]
myWorkspaces = clickable . (map xmobarEscape)
                $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

--myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- ToggleStruts
    , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++

    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
-- myLayout = smartBorders (tiled ||| Full)
myLayout = avoidStruts $smartBorders (tiled ||| Full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore ]

myEventHook = mempty

myLogHook = return ()
 
myStartupHook = do
     --spawnOnce "nitrogen --restore &"
     spawnOnce "nm-applet &"
     spawnOnce "volumeicon &"
     spawnOnce "udiskie &"
     --spawnOnce "picom &"
     spawnOnce "trayer --edge top --align right --widthtype request --padding 4 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x18191a --height 18 &"
     --spawnOnce "feh --randomize --bg-fill ~/Pictures/Wallpapers &"
     --spawnOnce "~/.fehbg &"  -- set last saved feh wallpaper &"
     spawnOnce "feh --bg-fill ~/Pictures/Wallpapers/0155.jpg &"
 
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ docks defaults
      { logHook     = dynamicLogWithPP xmobarPP
                        { ppOutput = hPutStrLn xmproc
                        , ppTitle = xmobarColor "green" "" . shorten 50
                        }
      }                 
      `additionalKeysP`
      [ ("M-w", spawn "firefox")
      , ("M-f", spawn "thunar")
      , ("M-l", spawn "libreoffice")
      , ("M1-m", spawn "thunderbird")
      , ("M-S-x", spawn "xterm")
      , ("C-a", treeselectAction tsDefaultConfig)
      , ("M-c", calcPrompt myXPConfig "qalc")
      , ("M-g", promptSearch myXPConfig google) 
      , ("M1-g", promptSearch myXPConfig amazon)
      , ("M-d", promptSearch myXPConfig duckduckgo)
      , ("M-a", promptSearch myXPConfig archwiki)
      , ("M1-x", promptSearch myXPConfig yahoo) 
      , ("M-o", shellPrompt myXPConfig)
      , ("M-z", spawn "gmrun")
      , ("M1-s", goToSelected myGSConfig1)
      , ("M-s", spawnSelected myGSConfig ["firefox","alacritty","thunar","thunderbird",
      "libreoffice","gimp","nitrogen","simple-scan","geany","xterm","gmrun",
      "lxappearance","nm-connection-editor","blueman-manager","system-config-printer"])
      ]
     
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

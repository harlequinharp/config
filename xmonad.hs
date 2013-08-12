import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import System.IO

modm = mod1Mask -- so we can change it in one location

myManageHook = composeAll [ 
    className =? "Pidgin" --> doShift "2",
    className =? "Thunderbird" --> doShift "3"
    ]

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0"] 

numPadKeys = [ 
    xK_KP_End,   xK_KP_Down,     xK_KP_Page_Down, -- 1, 2, 3
    xK_KP_Left,  xK_KP_Begin,    xK_KP_Right,     -- 4, 5, 6
    xK_KP_Home,  xK_KP_Up,       xK_KP_Page_Up,   -- 7, 8, 9
    xK_KP_Insert                                  -- 0
] 

tp_toggle = "/home/allie/config/scripts/trackpad_toggle.sh"

myKeys = [
    ((modm .|. shiftMask, xK_r), spawn tp_toggle),
    ((modm .|. shiftMask, xK_w), spawn "firefox"),
    ((modm .|. shiftMask, xK_f), spawn "thunar"),
    ((modm .|. shiftMask, xK_p), spawn "pidgin"),
    ((m .|. modm, k), windows $ f i) 
        | (i, k) <- zip myWorkspaces numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
]

main = do 
    xmproc <- spawnPipe "xmobar"
    xmonad $ defaultConfig { 
        manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig,
        layoutHook = avoidStruts $ layoutHook defaultConfig,
        logHook = dynamicLogWithPP xmobarPP { 
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        },
        terminal = "xfce4-terminal" ,
        modMask = modm, -- rebind mod to whatever modm is 
        focusFollowsMouse = False
    } `additionalKeys` myKeys
    

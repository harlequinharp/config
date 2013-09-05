import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Util.Run
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutHints
import System.IO

main = do
    xmproc <- spawnPipe "/home/allie/.cabal/bin/xmobar"
    xmonad $ defaultConfig {
        manageHook = manageDocks,
        layoutHook = layoutHints $ avoidStruts $ layoutHook defaultConfig,
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "brown" ""
        },
        terminal = "xfce4-terminal" ,
        focusFollowsMouse = False
    }


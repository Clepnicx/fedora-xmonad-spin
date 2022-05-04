import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spacing
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO

-- Xmonad Main
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad . ewmh . docks $ defaultConfig {
        terminal           = myTerminal,
        modMask            = myModKey, 
        borderWidth        = myBorderWidth, 
        normalBorderColor  = myNormalBorderColor, 
        focusedBorderColor = myFocusedBorderColor,  
        focusFollowsMouse  = myFocusFollowsMouse, 
        workspaces         = myWorkSpaces, 

        -- hooks, layouts
        startupHook        = myStartupHook, 
        layoutHook         = myLayoutHook, 
        logHook            = myLogHook xmproc 
    } `additionalKeys` myKeys



myTerminal           = "kitty"
myFileBrowser        = "dolphin"
myModKey             = mod4Mask    -- set Win-Key as ModKey
myFocusFollowsMouse  = True
myBorderWidth        = 2
myNormalBorderColor  = "#073dc1"
myFocusedBorderColor = "#ffffff"

-- Workspaces
myWorkSpaces  = ["1:kitty", "2:www", "3:files", "4", "5", "6", "7", "8", "9:misc"] 

-- Hooks, Layouts
myStartupHook = do
    spawn "picom -b"
    spawn "feh --bg-fill --no-fehbg ~/fedora-xmonad-build/wallpaper.png"

myLayoutHook  = avoidStruts  $  spacing 3 $ Tall 1 (3/100) (1/2)

myLogHook b   = dynamicLogWithPP xmobarPP { 
    ppOutput = hPutStrLn b
}

-- Key bindings
myKeys :: [((ButtonMask, KeySym), (X ()))]
myKeys = [
    ((mod4Mask, xK_w), spawn "firefox"), 
    ((mod4Mask, xK_d), spawn "rofi -show drun -font 'hack 10'"), 
    ((mod4Mask, xK_n), spawn myFileBrowser)
    ]

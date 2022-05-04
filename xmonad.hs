import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO

-- Xmonad Main
main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ docks defaultConfig {
        terminal           = myTerminal,
        modMask            = myModKey, 
        borderWidth        = myBorderWidth, 
        normalBorderColor  = myNormalBorderColor, 
        focusedBorderColor = myFocusedBorderColor,  
        focusFollowsMouse  = myFocusFollowsMouse, 
        workspaces         = myWorkSpaces, 

        -- hooks, layouts
        layoutHook         = myLayoutHook, 
        logHook            = myLogHook xmproc 
    } `additionalKeys` myKeys


-- User variables
myTerminal           = "kitty"
myFileBrowser        = "dolphin"
myModKey             = mod4Mask    -- set Win-Key as ModKey
myFocusFollowsMouse  = True
myBorderWidth        = 2
myNormalBorderColor  = "#8a2be2"
myFocusedBorderColor = "#fa7610"

-- Workspaces
myWorkSpaces  = ["1:kitty", "2:www", "3:files", "4", "5", "6", "7", "8", "9:misc"] 

-- Hooks, Layouts
{-- TODO: 
    - Check für doppelt belegte Tastenkombinationen hinzufügen
    - Spaceing zwischen Fenstern und Bar hinzufügen
--} 
myLayoutHook  = avoidStruts  $  layoutHook defaultConfig . spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $ Tall (1 (3/100) (1/2)) ||| Full
myLogHook b   = dynamicLogWithPP xmobarPP { 
    ppOutput = hPutStrLn b, 
    ppTitle = xmobarColor "green" "" . shorten 50
}

-- Key bindings
myKeys :: [((ButtonMask, KeySym), (X ()))]
myKeys = [
    ((mod4Mask, xK_w), spawn "firefox"), 
    ((mod4Mask, xK_d), spawn "rofi -modi drun,combi,window,ssh -font 'hack 10' -show combi"), 
    ((mod4Mask, xK_n), spawn myFileBrowser)
    ]

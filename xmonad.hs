import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spiral
import qualified Data.Map        as M
import Data.Monoid (mappend)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W
import XMonad.Actions.MouseResize
import XMonad.Layout.WindowArranger
import XMonad.Hooks.ManageHelpers (isFullscreen, isDialog,  doFullFloat, doCenterFloat)

myManageHook = composeAll
               [ resource  =? "desktop_window" --> doIgnore
               , resource =? "gimp"            --> doFloat
               , resource =? "mplayer"         --> doFloat
               , className =? "terminator"     --> doShift "1:terminal"
               , className =? "chromium"       --> doShift "2:www"
               , className =? "tor-browser"    --> doShift "3:tor"
               , className =? "rdesktop"       --> doShift "4:rdp"
               , className =? "emacs"          --> doShift "5:editor"
               , className =? "pidgin"         --> doShift "8:chat"
               , className =? "stalonetray"    --> doIgnore
               , isFullscreen                  --> (doF W.focusDown <+> doFullFloat)]

myTerminal = "terminator"
myModMask = mod4Mask
myLayout = mouseResize $ windowArrange $ layoutHook defaultConfig
myWorkspaces = ["1:terminal","2:www","3:tor","4:rdp","5:editor","6:email","7:files","8:chat"]


main :: IO()
main = xmonad $ewmh defaultConfig
       { modMask = myModMask
       , terminal = myTerminal
       , layoutHook = myLayout
       , keys = myKeys
       , workspaces = myWorkspaces
       }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Start a terminal.  Terminal to start is specified by supoTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Close client
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Sleep computer
  , ((modMask .|. controlMask, xK_s),
     spawn "slimlock; systemctl hybrid-sleep")

  -- Lock the screen using slimlock.
  , ((modMask .|. controlMask, xK_l),
     spawn "slimlock")

  -- Use this to launch programs without a key binding.
  , ((modMask, xK_space),
     spawn supoDmenuCmd)

  -- Fn key labeled with screen brightness decrease
  , ((0, 0x1008ff03),
     spawn "xcalib -co 90 -a")

  -- Fn key labeled with screen brightness increase
  , ((0, 0x1008ff02),
     spawn "xcalib -c")

  -- Fn key labeled with mute/unmute symbol
  , ((0, 0x1008FF12),
     spawn "amixer -q set Master toggle")

  -- Fn key labeled with volume decrease
  , ((0, 0x1008FF11),
     spawn "amixer -q set Master 5%-")

  -- Fn key labeled with volume increase
  , ((0, 0x1008FF13),
     spawn "amixer -q set Master 5%+")

  -- Put contents from primary selection into X selection
  , ((modMask .|. shiftMask, xK_b), spawn "xsel -op | xsel -ib")

  -- -- Decrement brightness
  -- , ((0, 0x1008FF04), spawn "echo 15 | sudo tee /sys/class/backlight/acpi_video0/brightness")
  -- --  , ((0, xF86XK_KbdBrightnessUp), spawn "brightness-up")

  -- -- Increment brightness
  -- , ((0, 0x1008FF05), spawn "echo 100 | sudo tee /sys/class/backlight/acpi_video0/brightness")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_p),
    sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
    setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
    refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
    windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
    windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
    windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
    windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
    windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
    windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
    windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
    sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
    sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
    withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
    sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
    sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  --, ((modMask .|. shiftMask, xK_q),
   -- io exitSuccess)

  -- Take screenshot
  , ((modMask .|. shiftMask, xK_u),
    spawn "screenshot-workspace | xargs imgur-upload | xargs firefox -new-tab")

  -- Restart xmonad.
  , ((modMask, xK_q),
    restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

supoDmenuFg = "'#d8d8d8'"
supoDmenuBg = "'#111111'"
supoDmenuFont = "'-*-lucida-bold-*-*-*-34-*-*-*-*-*-*-*'"

supoDmenuCmd =
  unwords [ "dmenu_run"
    , "-nb"
    , supoDmenuBg
    , "-nf"
    , supoDmenuFg
    , "-sf"
    , "'#000000'"
    , "-fn"
    , supoDmenuFont
  ]

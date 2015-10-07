import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.Spiral
-- import XMonad.Actions.Volume
import qualified Data.Map        as M
import Data.Monoid (mappend)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.StackSet as W

myManageHook = composeAll
               [ resource =? "gimp" --> doFloat
               , resource =? "mplayer" --> doFloat
               ]

myTerminal = "urxvt"
myModMask = mod4Mask
myLayout = spiral(6/7) ||| Full
--myKeys conf@(XConfig) = fromList $
--  [
--
--  ]


main :: IO()
main = xmonad $ewmh defaultConfig
       { modMask = myModMask
       , terminal = myTerminal
       , layoutHook = myLayout
       , keys = myKeys
       }

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  -- Start a terminal.  Terminal to start is specified by supoTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Sleep computer
  , ((modMask .|. controlMask, xK_s),
     spawn "slimlock; systemctl hybrid-sleep")

  -- Lock the screen using slimlock.
  , ((modMask .|. controlMask, xK_l),
     spawn "slimlock")

  -- Use this to launch programs without a key binding.
  , ((modMask, xK_space),
     spawn supoDmenuCmd)

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

  -- Decrement brightness
  , ((0, xF86XK_KbdBrightnessDown), spawn "brightness-down")

  -- Increment brightness
  , ((0, xF86XK_KbdBrightnessUp), spawn "brightness-up")

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
Just a quickie to share a tweak I had to make to my `xmonad.hs`. Not sure if there's a better way to do this, but hey.

The goal was to finally, actually get working hibernation on my laptop. I usually use it in short bursts, so I just got used to shutting it down between sessions. However, I recently started using a work laptop running Windows 7 and hibernation has been useful there<a name="note-Mon-Oct-21-121457EDT-2013"></a>[|1|](#foot-Mon-Oct-21-121457EDT-2013), and I'll be damned if the non-free shitbox is going to have a mildly useful feature that my machine doesn't.

The way you get a Debian machine to hibernate or suspend is with the appropriately named `pm-hibernate` and `pm-suspend` commands<a name="note-Mon-Oct-21-121501EDT-2013"></a>[|2|](#foot-Mon-Oct-21-121501EDT-2013), so I figured this would be a fairly easy key binding

```haskell
...
, ("C-t C-<Delete>", spawn "pm-suspend")
, ("C-t <Delete>", spawn "pm-hibernate")
...
```

Unfortunately, the `pm-*` are `root` user commands. And Xmonad doesn't automatically prompt for a password when you do something like `su -c pm-suspend`. *And*, unlike with `sudo`, you can't pass a password into `su`. So that approach is right out.

I googled around for alternatives for a little while, but What I ended up doing was finally adding myself to the `sudo` group, and defining this function for my own nefarious purposes

```haskell
sudoSpawn command = withPrompt "Password" $ run command
  where run command password = spawn $ concat ["echo ", password, " | sudo -S ", command]
```

`withPrompt` is another little utility piece I had written for some desktop changers; it's defined as

```haskell
withPrompt prompt fn = inputPrompt xpConf prompt ?+ fn
```

The above defined, I can now bind super-user commands to Xmonad keystrokes

```haskell
...
, ("C-t C-<Delete>", sudoSpawn "pm-suspend")
, ("C-t <Delete>", sudoSpawn "pm-hibernate")
...
```

with the caveat that I need to enter my password each time I invoke these. I'll see if that's too annoying. Worst case scenario, I'll pull some trickery to cache it the first time I enter it.

Just in case you care, my complete `xmonad.hs` now looks like:

```haskell
import System.Directory
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.WindowGo
import XMonad.Actions.GridSelect
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Util.EZConfig
import XMonad.Util.CustomKeys

import qualified XMonad.StackSet as S

main = xmonad $ conf
       `additionalKeysP`
       [ ("C-t C-d C-b", withFilePrompt "Pic: " bgFolder setDesktopBackground)
       , ("<Print>", withFilePrompt "Name: " screenshotFolder capToFile)
         
       , ("C-t C-<Delete>", sudoSpawn "pm-suspend")
       , ("C-t <Delete>", sudoSpawn "pm-hibernate")
         
       , ("C-t p", spawn "dmenu_run")
       , ("C-t C-p", spawn "dmenu_run")
       , ("C-t <Return>", spawn "xterm")
       , ("C-t e", runOrRaise "emacs" (className =? "Emacs"))
       , ("C-t C-e", runOrRaise "emacs" (className =? "Emacs"))
       , ("C-t b", spawn "chromium --proxy-server=\"socks://localhost:9050\" --incognito")
       , ("C-t C-b", spawn "chromium --proxy-server=\"socks://localhost:9050\" --user-agent=\"Mozilla/5.0 (Windows NT 5.1) AppleWebKit/537.4 (KHTML, like Gecko) Chrome/22.0.1229.94 Safari/537.4\"")
         
       , ("C-t s", nextWS)
       , ("C-t C-s", prevWS)
       , ("C-t w", toggleWS)
       , ("C-t C-w", toggleWS)
       , ("C-t C-t", windowSwap)
       , ("C-t t", windows S.swapDown)
       , ("C-t C-j", windows S.swapDown)
       , ("C-t j", windows S.focusDown)
       , ("C-t k", windows S.focusUp)
       , ("C-t C-k", windows S.swapUp)
       , ("C-t g", goToSelected defaultGSConfig)
         
       , ("C-t C-<Space>", sendMessage NextLayout)
       , ("C-t C-h", sendMessage Shrink)
       , ("C-t C-l", sendMessage Expand)
         
       ]
  where conf = defaultConfig { XMonad.startupHook = onStartup, modMask = mod4Mask }

---------- Config Options
bgFolder = "/home/inaimathi/pictures/backgrounds/"
screenshotFolder = "/home/inaimathi/pictures/screenshots/"

onStartup :: X ()
onStartup = do
  spawn "set-monitors"
  spawn "pmount /dev/mmcblk0p1"
  setDesktopBackground "edge-of-the-world.jpg"

---------- Helper Functions
setDesktopBackground :: MonadIO m => String -> m ()
setDesktopBackground pic = spawn $ concat ["feh --no-xinerama --bg-fill ", bgFolder, pic]
        
capToFile :: MonadIO m => String -> m ()
capToFile picName = spawn $ concat ["import ", screenshotFolder, picName]

sudoSpawn command = withPrompt "Password" $ run command
  where run command password = spawn $ concat ["echo ", password, " | sudo -S ", command]

---------- Utility
windowSwap = do
  windows S.focusDown
  windows S.swapUp

xpConf = defaultXPConfig { position = Top }

withPrompt prompt fn = inputPrompt xpConf prompt ?+ fn

withCompletingPrompt prompt completions fn = 
  inputPromptWithCompl xpConf prompt comp ?+ fn
  where comp = mkComplFunFromList completions

withFilePrompt prompt directory fn = do
  files <- liftIO $ getDirectoryContents directory
  let fs = filter relevant files
      relevant f = '.' /= head f
  withCompletingPrompt prompt fs fn
```


* * *
##### Footnotes

1 - <a name="foot-Mon-Oct-21-121457EDT-2013"></a>[|back|](#note-Mon-Oct-21-121457EDT-2013) - Granted, because the boot time on that machine is something like 5 minutes instead of the 12 seconds I'm used to waiting, Hibernate is a goddamn necessity, but I digress.

2 - <a name="foot-Mon-Oct-21-121501EDT-2013"></a>[|back|](#note-Mon-Oct-21-121501EDT-2013) - Ideally, I'd just be using hibernate, but there are some issue. I've upgraded my ram since installing the OS, which means that my `swap` partition isn't big enough to store a memory dump, and I can't seem to resize it with `gparted`, with or without `swapoff`/`swapon` magic. Luckily, I've had a larger hard drive waiting for me to crack open the box and configure it to my liking, so I'll just do that this week rather than procrastinating. In the meantime though, I'm `suspend`ing instead.

The first part is exposition. If you're just interested in how to set up Screen as a StumpWM analogue, skip to the next heading.

I've been thinking about window management again, for my own purposes and bouncing around between combination of Xmonad, StumpWM, XFCE and GNOME (as well as trying the two tiling managers standalone). 

Using XFCE and GNOME standalone really wasn't going to do anything for me, I already knew that. Using the lightweights on *theiir* own had a few minor annoyances that I tried to fix by running them together.

Out of the box, neither Xmonad nor StumpWM


-   support a nautilus/thunar style file display (and I sometimes need it)
-   auto-connect to my wireless network
-   automatically mount external media (or watch for new drives being added and mount them as necessary)


It's becoming clear that I don't want a regular point-and-click interface by default anymore, except for one or two specialized tasks for which `nautilus --no-desktop` should suffice.

I also don't really use removable media anymore. Maybe my memory is a bit clouded, but it seems that I used *a lot* more USB keys, DVDs and CDs back when I was a Windows/OS X user. It's possible that I was just being stupid, but it seemed like the easiest way of sharing data between two different machines<a name="note-Thu-Oct-06-092559EDT-2011"></a>[|1|](#foot-Thu-Oct-06-092559EDT-2011). That flat out doesn't happen anymore. We only have Linux machines in the house now (split between Debian, Parabola and Ubuntu, in order of descending quantity), so when I want to share data between them, I use `scp`, or possibly `rsync` depending on the specific situation. I don't do backups to DVD or CD anymore; I just use hard drives and the only computer that needs to play DVD media is in the livingroom<a name="note-Thu-Oct-06-092727EDT-2011"></a>[|2|](#foot-Thu-Oct-06-092727EDT-2011). I also don't install things from CDs, except for Debian itself.

Finally, connecting to my wireless network isn't automatically handled, and I do still need to do that with my netbook, but I can [work around it](https://github.com/Inaimathi/shell-ui/blob/master/ruby/wlan)<a name="note-Thu-Oct-06-112837EDT-2011"></a>[|3|](#foot-Thu-Oct-06-112837EDT-2011). Granted, I could have just memorized how to do it via `iwconfig` and friends, but this way is simpler from the interface perspective.

Bottom line; I don't need a desktop environment anymore. I'm good with the plain window manager. So it looks like GNOME is coming off my own desktop this weekend and Stump is getting re-instated as the manager of choice. The thing is, I also have a few old machines lying around that chug noticeably under any sort of graphic interface. And it turns out that if I'm willing to ditch nautilus, and fend for myself in terms of mounting media/connecting to networks, then I can go all the way to terminal.

I've been using [GNU Screen](http://www.gnu.org/s/screen/) as a way of deploying Lisp applications, but looking over the [keybindings](http://www.gnu.org/software/screen/manual/html_node/Default-Key-Bindings.html) and `man` page, it looks like it can serve as a respectable alternative to a tiling window manager.

### <a name="screen-wm" href="#screen-wm"></a>Screen WM

The default control combination is `C-a` instead of `C-t`, and the keys are significantly different, and you can't extend it in Lisp<a name="note-Thu-Oct-06-114732EDT-2011"></a>[|4|](#foot-Thu-Oct-06-114732EDT-2011), but it looks like a fairly simple `.screenrc` file can turn it into Stump-Lite. Here's a quick breakdown, assuming the default bindings:


-   `C-a ?` shows you the help screen. 
-   `C-a c` starts a new terminal in the same session (when you re-attach later, you'll have both of these)
-   `C-a |` splits the screen vertically (note that screen doesn't automatically start a second terminal). Equivalent to `C-t S`
-   `C-a S` splits screen horizontally. Equivalent to `C-t s`
-   `C-a TAB` moves to the next split
-   `C-a X` removes the current split
-   `C-a C-a` pulls the other terminal. Equivalent to `C-t C-t`
-   `C-a n`/`C-a C-n` cycles to the next terminal (`C-a p`/`C-a C-p` cycles backward)


In other words, out of the box, you've got the same basic window management shortcuts this way. And if you feel like remembering extra keys, feel free to commit the above to memory. As for me, my `.screenrc` file is going to look something like

```

startup_message off

bind S split -v
bind s split
bind R remove
bind ^e screen emacs -nw
```

On a machine where I plan to use terminal exclusively, I'll also add

```
escape ^t
bind ^t other
```

to mirror the StumpWM keys I'm already used to.

Incidentally, that last line in part one is what got me convinced that `screen` could credibly replace [X](http://en.wikipedia.org/wiki/X_Window_System) for my purposes (assuming I'm working anywhere other than my dual-screen setup). It seems like you can wire up arbitrary shell commands and bind them to keypresses (use `exec` instead of `screen` if you don't want to start a new window for them). I left it out, but you can also put regular `screen` calls in `.screenrc` like so

```
split
resize 60
screen -t lynx lynx
screen -t emacs emacs -nw
focus
screen -t top top
focus
```

in order to customize your startup routine. I'm sure I could get more complex than that, but it illustrates the point. That snippet starts me off with a horizontal split. The top frame is `emacs`, the bottom frame is `top` and `lynx` is running in the background.

The stuff that I'll be missing this way is


-   A `dmenu`-like command (it seems like you can't have screen prompt for user input to then use in a keybinding; I'll have to do more research. The only thing I'd do with this is setup some `lynx` webjumps in any case.)
-   X windows (so no GIMP, `gitk` or a graphic browser on my dev machine, which is actually a good thing on balance since that'll reduce Reddit use)
-   Resizing mode (you *can* resize windows in screen, but you do it by typing in a height/width in lines/cols to set the width to, rather than the Stump resize mode where you can incrementally tweak windows)
-   A `run-or-raise` equivalent (the `C-t C-e` binding as above will actually start a new `emacs` every time rather than switching to it if one already exists)


Given how my `.stumpwmrc` is shaping up, I don't think this'll be a big sacrifice. The thing I think I'll miss most is actually `gitk`. I'll let you know how it goes.

* * *
##### Footnotes

1 - <a name="foot-Thu-Oct-06-092559EDT-2011"></a>[|back|](#note-Thu-Oct-06-092559EDT-2011) - Whether they were both mine and sitting in my room, or not and lying on a table in the OCAD student lounge.

2 - <a name="foot-Thu-Oct-06-092727EDT-2011"></a>[|back|](#note-Thu-Oct-06-092727EDT-2011) - And has a standard GNOME 2 setup out of deference to my wife, who hasn't taken the Computer Nerd prestige class remaining a regular nerd.

3 - <a name="foot-Thu-Oct-06-112837EDT-2011"></a>[|back|](#note-Thu-Oct-06-112837EDT-2011) -  Been meaning to do a writeup on that little UI layer I'm slowly using to coat my shell experience. [pack](https://github.com/Inaimathi/shell-ui/blob/master/ruby/pack) and [unpack](https://github.com/Inaimathi/shell-ui/blob/master/ruby/unpack) have already left me smiling several times.

4 - <a name="foot-Thu-Oct-06-114732EDT-2011"></a>[|back|](#note-Thu-Oct-06-114732EDT-2011) - :(

# xbronad

[DISCLAIMER] I use us(colemak) instead of qwerty, so you'll likely want to modify the keybindings to suit you better.
If someone wants to contribute a dynamic key-location-on-keyboard solution that would be appreciated. Without `XMonad.Actions.KeyRemap` peferrably, as it doesn't seems to support additionalKeysP. Although, that could be modded :thonk:... That solution still probably wouldn't be nice because people not using colemak would have a hard time rebinding these to what they want. If not, I'll probably get around to it **eventually**.

An xmonad config that attempts to stand out from all the rest, while keeping end-user simplicity.

Based on [DT's config](https://gitlab.com/dwt1/dotfiles/-/tree/master/.xmonad), however, it's blossomed into much much more.

> For simple reconfiguring look at [lib/Custom/Vars.hs](https://github.com/Techtiger255/xbronad/blob/master/lib/Custom/Vars.hs)

PLEASE look at [lib/Custom/Keys.hs](https://github.com/Techtiger255/xbronad/blob/master/lib/Custom/Keys.hs) for the keybindings.  
They're simple, but most defaults are overriden -- help messages are strewn about, but it helps to know what they are before diving in.

```
#!/bin/cursed-lang
let
  M  = (Super || Meta || Command || Winblows) key
  M1 = Alt key
  C  = Control key
  S  = Shift key
if (you get stuck); then
  try:
    hitting "M-\ ]"
    # this is a quick way to enter common configs.  
  except:  # if for some reason that doesn't work:
    try:
      hitting "C-M1-f3"
      # C-M1-f[1,2,3,4,5,6,7,8,9...] changes virtual TTY's
      # if one of your sessions runs into an error, try changing TTY's,
      # launching a new one, and killing the old TTY session. The issue
      # may be caused by Dbus in your .xinitrc, try commenting that out,
      # and try replicating what you did. For me it was media keys, most of
      # them didn't work, but without Dbus they did!
    except:  # if none of that works
      try:
        "turning it off and on again"
      except:
        consult internet  # for help
fi
```
Prefferred programs can be located in Vars.hs.

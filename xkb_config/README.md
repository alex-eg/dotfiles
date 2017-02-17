Install
--------

Copy the contents to `~/.config/xkb`.


Compile and apply
--------

To immediately apply new map, run the following command:


    xkbcomp -I$HOME/.config/xkb ~/.config/xkb/keymap/mymap $DISPLAY


And to run it every time your window manager starts, add it to `.xinitrc`, or
just to your WM of choise default autostart mechanism.

#!/bin/sh

userresources=${HOME}/.Xresources
usermodmap=${HOME}/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi
if [ "$HOST" = "desk" ]; then
    xrdb -merge "$HOME/.Xcolors/paper_color"
else
     xrdb -merge "$HOME/.Xcolors/gruvbox"
     xrdb -merge "$HOME/.Xcolors/gruvbox_urxvt256"
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
    for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi


#on archy-mac


#keyboard repeat rate {Delay Repeat-Speed}
xset r rate 200 35

# pulseaudio --start
#exec wicd-gtk
#exec xterm -geometry 80x66+0+0 -name login

#HiDPI settings
# xrandr --fbmm 286x179
# xrandr --output DP1 --auto --left-of eDP1

###############
#Ideal Desktop#
###############

exec i3

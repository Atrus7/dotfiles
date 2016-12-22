# arch-specific
emulate sh -c 'source /etc/profile'


# start xorg
[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx

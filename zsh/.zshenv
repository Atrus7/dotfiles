export PATH=$HOME/bin:/usr/sbin:$HOME/.local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/bin:/sbin

# assuming chromium is installed...
export PATH=$PATH:$HOME/code/chromium/depot_tools

export BROWSER=chromium
export EDITOR='emacsclient -c --alternate-editor=""'
export VISUAL='emacs --no-desktop'
export ALTERNATE_EDITOR=vim
export TERM=rxvt-unicode

#languages
export LC_COLLATE=en_US.UTF-8
export LC_CTYPE=en_US.UTF-8
export LC_MESSAGES=en_US.UTF-8
export LC_MONETARY=en_US.UTF-8
export LC_NUMERIC=en_US.UTF-8
export LC_TIME=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8
export LANGUAGE=en_US.UTF-8
export LESSCHARSET=utf-8

export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

export CC="`which gcc` -fdiagnostics-color=auto"
export CXX="`which g++` -fdiagnostics-color=auto"

# make Chrome scaling normal (also needs --force-device-scale-factor)
export GDK_SCALE=1

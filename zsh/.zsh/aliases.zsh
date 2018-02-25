alias ls="ls -hF --color=auto"
alias lsl="ls -lhF --color=auto"
alias ll="ls -lahF --color=auto"
alias fuck='sudo $(fc -ln -1)'
alias emacs_kill="emacsclient -e '(kill-emacs)'"
alias emacs_debug="emacs --debug-init"
alias emacs_fresh="emacs --no-desktop"
alias ec="emacsclient -c"
alias e=$EDITOR
#alias connect_vpn="sudo openvpn ........"

alias cstwins_update="ssh cstwins 'cd /var/www/html/cstwins/; git pull; sudo apachectl restart'"

#sudo commands
alias pacman="sudo pacman"
alias pip="sudo pip"
alias :qa="sudo systemctl poweroff"
alias reboot="sudo reboot"
alias poweroff="sudo poweroff"

alias "cd ..."="cd ../.."
alias "cd ...."="cd ../../.."
alias "cd ....."="cd ../../../.."

alias gs="git status"

echocolors() {
    #   This file echoes a bunch of color codes to the
    #   terminal to demonstrate what's available.  Each
    #   line is the color code of one forground color,
    #   out of 17 (default + 16 escapes), followed by a
    #   test use of that color on all nine background
    #   colors (default + 8 escapes).

    T='gYw'   # The test text

    echo -e "\n                 40m     41m     42m     43m     44m     45m     46m     47m"

    for FGs in '    m' '   1m' '  30m' '1;30m' '  31m' '1;31m' '  32m' '1;32m' '  33m' '1;33m' '  34m' '1;34m' '  35m' '1;35m' '  36m' '1;36m' '  37m' '1;37m'
    do FG=${FGs// /}
      echo -en " $FGs \033[$FG  $T  "
      for BG in 40m 41m 42m 43m 44m 45m 46m 47m;
      do echo -en "$EINS \033[$FG\033[$BG  $T  \033[0m";
      done
      echo;
    done
    echo
  }

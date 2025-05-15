HISTFILE=~/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000

# Make sure history from all sessions is shared and appended
setopt APPEND_HISTORY             # Don't overwrite history
setopt SHARE_HISTORY              # Share with other sessions
setopt HIST_IGNORE_ALL_DUPS       # Avoid clutter from duplicates
setopt HIST_REDUCE_BLANKS         # Remove unnecessary whitespace
setopt EXTENDED_HISTORY           # Include timestamp and duration

alias history='fc -rl 1' # so i can do things like history | grep ssh


# Ctrl+R: reverse-i-search using fzf
autoload -Uz up-line-or-beginning-search
bindkey '^R' fzf-history-widget

fzf-history-widget() {
    BUFFER=$(fc -rl 1 | fzf --tac | sed 's/^[ 0-9]*//')
    CURSOR=$#BUFFER
    zle reset-prompt
}
zle -N fzf-history-widget

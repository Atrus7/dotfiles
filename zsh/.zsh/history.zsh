HISTFILE=~/.zhistory
HISTSIZE=1000000
SAVEHIST=1000000

# Make sure history from all sessions is shared and appended
setopt APPEND_HISTORY             # Don't overwrite history
setopt SHARE_HISTORY              # Share with other sessions
setopt HIST_REDUCE_BLANKS         # Remove unnecessary whitespace
setopt EXTENDED_HISTORY           # Include timestamp and duration
setopt HIST_EXPIRE_DUPS_FIRST     # Let zsh expire older dups first when trimming history

alias history='fc -rl 1' # so i can do things like history | grep ssh

# Ctrl+R: reverse-i-search using fzf
autoload -Uz up-line-or-beginning-search
bindkey '^R' fzf-history-widget

# Tab means Complete, Enter means "Do this now."
fzf-history-widget() {
    local key
    local selected_cmd

    # Use fc with line numbers; fzf strips with sed; --expect allows key capture
    key_and_cmd=$(fc -rl 1 | fzf --scheme=history --tiebreak=index --expect='enter,tab' --bind 'tab:accept')
    key=$(echo "$key_and_cmd" | head -n1)
    selected_cmd=$(echo "$key_and_cmd" | sed -n '2s/^[[:space:]]*[0-9]\+[[:space:]]//p')
    # selected_cmd=$(echo "$key_and_cmd" | sed -n '2{p;q}')

    if [[ -z "$selected_cmd" ]]; then
        zle reset-prompt
        return
    fi

    if [[ "$key" == "tab" ]]; then
        BUFFER="$selected_cmd"
        CURSOR=${#BUFFER}
        zle reset-prompt
    elif [[ "$key" == "enter" ]]; then
        BUFFER="$selected_cmd"
        CURSOR=${#BUFFER}
        zle accept-line
    fi
}
zle -N fzf-history-widget
bindkey '^R' fzf-history-widget

zle -N fzf-history-widget

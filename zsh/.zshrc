
[[ -o interactive ]]; then
    #Load Zsh files
    for config (~/.zsh/*.zsh) source $config
else
    # if tramp....don't do fancy stuff
    # [[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return
    [[ $EMACS = t ]] && unsetopt zle
fi

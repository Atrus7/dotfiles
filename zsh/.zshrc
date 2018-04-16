# if tramp....don't do fancy stuff
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

#Load Zsh files
for config (~/.zsh/*.zsh) source $config

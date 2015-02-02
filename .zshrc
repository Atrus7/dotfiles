
##
# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh

MODE_INDICATOR="%{$fg_bold[blue]%}[%{$fg[blue]%}NORMAL]%{$reset_color%}"
bindkey -M viins 'jk' vi-cmd-mode
# bindkey '^P' up-history
# bindkey '^N' down-history
# bindkey '^?' backward-delete-char
# bindkey '^h' backward-delete-char
# bindkey '^w' backward-kill-word
# bindkey '^r' history-incremental-search-backward

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
  # Set name of the theme to load.
  # Look in ~/.oh-my-zsh/themes/
  # Optionally, if you set this to "random", it'll load a random theme each
  # time that oh-my-zsh is loaded.
  ZSH_THEME="steeef"

  CASE_SENSITIVE="false"

  # Uncomment the following line to disable bi-weekly auto-update checks.
  DISABLE_AUTO_UPDATE="true"

  # Uncomment the following line to change how often to auto-update (in days).
  #export UPDATE_ZSH_DAYS=14

  # Uncomment the following line to disable colors in ls.
  # DISABLE_LS_COLORS="true"

  # Uncomment the following line to disable auto-setting terminal title.
  # DISABLE_AUTO_TITLE="true"

  # Uncomment the following line to enable command auto-correction.
  # ENABLE_CORRECTION="true"

  # Uncomment the following line to display red dots whilst waiting for completion.
  COMPLETION_WAITING_DOTS="true"

  # Uncomment the following line if you want to disable marking untracked files
  # under VCS as dirty. This makes repository status check for large repositories
  # much, much faster.
  # DISABLE_UNTRACKED_FILES_DIRTY="true"

  # Uncomment the following line if you want to change the command execution time
  # stamp shown in the history command output.
  # The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
  # HIST_STAMPS="mm/dd/yyyy"

  # Would you like to use another custom folder than $ZSH/custom?
  # ZSH_CUSTOM=/path/to/new-custom-folder

  # Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
  # Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
  # Example format: plugins=(rails git textmate ruby lighthouse)
  # Add wisely, as too many plugins slow down shell startup.
  plugins=(git vi-mode)

  source $ZSH/oh-my-zsh.sh

  # User configuration

  export PATH=$HOME/bin:/usr/local/bin:$PATH
  # export MANPATH="/usr/local/man:$MANPATH"
  # You may need to manually set your language environment
  # export LANG=en_US.UTF-8
  #

  # Preferred editor for local and remote sessions
  # if [[ -n $SSH_CONNECTION ]]; then
  #else
  #  export EDITOR='vim'
  #fi
  export EDITOR='vim'

  # Compilation flags
  # export ARCHFLAGS="-arch x86_64"

  # ssh
  export SSH_KEY_PATH="~/.ssh/dsa_id"

  # Set personal aliases, overriding those provided by oh-my-zsh libs,
  # plugins, and themes. Aliases can be placed here, though oh-my-zsh
  # users are encouraged to define aliases within the ZSH_CUSTOM folder.
  # For a full list of active aliases, run `alias`.

  #Steve Losh was right: Fuck computers
  # Example aliases
  # alias zshconfig="mate ~/.zshrc"
  # alias ohmyzsh="mate ~/.oh-my-zsh"
  alias @sob="cd ~/Desktop/Desktop\ Directory/StudyOnBoard\ Code/Github\ Repositories/StudyOnBoard"
  alias @desk="cd ~/Desktop/Desktop\ Directory/"
  alias @allen="cd /Users/Chris/Desktop/Desktop\ Directory/Programming/Allen\ Webb\ Research/dns_statistics/chris_project/"
  alias @school="cd ~/Desktop/Desktop\ Directory/School/"
  alias @code="cd ~/Desktop/Desktop\ Directory/Programming/"
  alias @gpp= "g++ -std=c++11"
  alias @vimrc="vim ~/.vimrc"
  alias @zshrc="vim ~/.zshrc"
    alias ls='ls -a'
  #  alias @zshrc="vim ~/.zshrc"
  #  alias @hi="echo \"Hello World\" "

  DEFAULT_USER="Chris"

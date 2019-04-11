# Tab-completion

autoload -U compinit
compinit
zstyle ':completion:*:descriptions' format '%U%B%d%b%u'
setopt completealiases
zstyle ':completion:*' menu select

# Simple gentoo prompt
autoload -U promptinit
promptinit

if [[ -n $SSH_CONNECTION ]]; then
  prompt bart
else
  prompt off
fi

# http://stackoverflow.com/questions/1128496/to-get-a-prompt-which-indicates-git-branch-in-zsh
setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' formats       \
    '%F{5}[%F{2}%b%F{5}]%f'

zstyle ':vcs_info:*' enable git cvs svn

vcs_info_wrapper() {
  vcs_info
  if [ -n "$vcs_info_msg_0_" ]; then
    echo "%{$fg[grey]%}${vcs_info_msg_0_}%{$reset_color%}$del"
  fi
}

PROMPT='%F{red}❯%f%F{yellow}❯%f%F{green}❯%f '
echo -en "\033[6 q" # Make a cursor to be a vertical bar

bindkey -e

autoload -U select-word-style
select-word-style bash

zstyle ':completion:*' matcher-list '' \
  'm:{a-z\-}={A-Z\_}' \
  'r:[^[:alpha:]]||[[:alpha:]]=** r:|=* m:{a-z\-}={A-Z\_}' \
  'r:[[:ascii:]]||[[:ascii:]]=** r:|=* m:{a-z\-}={A-Z\_}'

autoload edit-command-line
zle -N edit-command-line
bindkey '^v' edit-command-line

export HISTSIZE=2000
export HISTFILE="$HOME/.history"
export SAVEHIST=$HISTSIZE
setopt hist_ignore_all_dups
setopt hist_ignore_space
setopt autocd
# setopt extendedglob # Extended regular expressions cp ^*.(tar.

alias -g L='| less '
alias -g H='| head '
alias -g T='| tail -n 1000 '
alias -g G='| grep '
alias -g S='| sed '
alias -g ts='date +%s'

# docker
alias drm='docker rm $(docker ps -a -q)'
alias drmi='docker rmi $(docker images -q --filter "dangling=true")'
alias di='docker images'
alias dps='docker ps'

# git
alias glg='git log --oneline --graph'
# remove local tags which not presented in remote repo
alias gpt='git fetch --prune origin "+refs/tags/*:refs/tags/*"'
alias gpl='git pull --rebase'
alias gm='git merge --no-ff'
alias gis='git status -s'
alias ga='git add'
alias gc='git commit'
alias gd='git diff'

alias ku='kubectl'

alias tm='tmux attach || tmux new'
alias vim='emacsclient'
alias vi='emacsclient -n -a ""'

# export EDITOR="~/.bin/ec"

if [[ -z "$TMUX" && -n "$SSH_CONNECTION" ]]; then
    tmux -2 attach -d
fi

case $TERM in
  (*xterm* | rxvt)

    function precmd {
      print -Pn "\e]0;zsh%L %(1j,%j job%(2j|s|); ,)%~\a"
    }

    function preexec {
      printf "\033]0;%s\a" "$1"
    }

  ;;
esac

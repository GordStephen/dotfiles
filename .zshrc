# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"
alias vm="vagrant"
alias tmux="tmux -2"
alias untgz="tar xvzf"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
plugins=(colorize colored-man git command-not-found copyfile npm nvm vagrant history history-substring-search)

. $ZSH/oh-my-zsh.sh

# Customize to your needs...

export TERM=xterm-256color
[ -n "$TMUX" ] && export TERM=screen-256color

export PATH=$PATH:/opt/node/bin:/usr/local/lib/node_modules/npm/node_modules:~/.cabal/bin
export NODE_PATH=$NODE_PATH:/usr/local/lib/node_modules:/usr/local/lib/node_modules/npm/node_modules
export PATH=$PATH:/home/gord/.sbt/bin
export PATH="`ruby -e 'puts Gem.user_dir'`/bin:$PATH"
export EDITOR=/usr/bin/vim
#export LC_ALL=en_US.utf8
#export LANG=en_US.utf8

csview()
{
  local file="$1"
  cat $file | sed -e 's/,,/, ,/g' | column -s, -t | less -#5 -N -S
}

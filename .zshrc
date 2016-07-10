#
# User configuration sourced by interactive shells
#

# Activate vi mode
bindkey -v
export KEYTIMEOUT=1

# Source zim
if [[ -s ${ZDOTDIR:-${HOME}}/.zim/init.zsh ]]; then
  source ${ZDOTDIR:-${HOME}}/.zim/init.zsh
fi

# Quick-and-dirty csv viewer
csview()
{
  local file="$1"
  cat $file | sed -e 's/,,/, ,/g' | column -s, -t | less -#5 -N -S
}

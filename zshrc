# Path to your oh-my-zsh configuration.
DOTFILES=$HOME/.dotfiles

# Path to your oh-my-zsh configuration.
ZSH=$DOTFILES/oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(git git-extras github osx django pip python redis-cli)
source $ZSH/oh-my-zsh.sh

export EDITOR="emacs -nw"

export PATH=/usr/local/share/python:/usr/local/bin:/usr/local/sbin:$HOME/.bin:$GEM_HOME/bin:$PATH:/usr/local/Cellar/node/7.10.0/bin
source $DOTFILES/secrets

PATH=$PATH:$HOME/.rvm/bin:$HOME/.gems/bin:$HOME/bin;

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

source $DOTFILES/localrc

# eval "$(direnv hook zsh)"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

alias emacs="emacs -nw"
alias prod='fab prod'
alias staging='fab staging'
alias activate='. venv/bin/activate'

# In order for gpg to find gpg-agent, gpg-agent must be running, and there must be an env
# variable pointing GPG to the gpg-agent socket. This little script, which must be sourced
# in your shell's init script (ie, .bash_profile, .zshrc, whatever), will either start
# gpg-agent or set up the GPG_AGENT_INFO variable if it's already running.

# Add the following to your shell init to set up gpg-agent automatically for every shell
if [ -f ~/.gnupg/.gpg-agent-info ] && [ -n "$(pgrep gpg-agent)" ]; then
    source ~/.gnupg/.gpg-agent-info 
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon ~/.gnupg/.gpg-agent-info > /dev/null 2>&1)
fi

function cd() {
  builtin cd "$@"

  if [[ -z "$VIRTUAL_ENV" ]] ; then
    ## If env folder is found then activate the vitualenv
      if [[ -d ./venv ]] ; then
        source ./venv/bin/activate
      fi
  else
    ## check the current folder belong to earlier VIRTUAL_ENV folder
    # if yes then do nothing
    # else deactivate
      parentdir="$(dirname "$VIRTUAL_ENV")"
      if [[ "$PWD"/ != "$parentdir"/* ]] ; then
        deactivate
      fi
  fi
}

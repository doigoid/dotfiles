# Path to your oh-my-zsh configuration.
DOTFILES=$HOME/.dotfiles

# Path to your oh-my-zsh configuration.
ZSH=$DOTFILES/oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(launchctl git git-extras git-remote-branch github osx django pip python redis-cli knife)
source $ZSH/oh-my-zsh.sh

export EDITOR=emacs
export WORKON_HOME=$HOME/.envs
export GEM_HOME=$HOME/.gems

export PATH=/usr/local/share/python:/usr/local/bin:/usr/local/sbin:$HOME/bin:$GEM_HOME/bin:$PATH
source $DOTFILES/secrets
export PYTHONPATH=/usr/local/lib/python2.7/site-packages:$PYTHONPATH

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python

VIRTUALENVWRAPPER="/usr/local/bin/virtualenvwrapper.sh"
if [ -f "$VIRTUALENVWRAPPER" ]; then
   source $VIRTUALENVWRAPPER;
fi

PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

has_virtualenv() {
    if [ -e .venv ]; then
        workon `cat .venv`
    fi
}
venv_cd () {
    cd "$@" && has_virtualenv
}
alias cd="venv_cd"

source $DOTFILES/localrc

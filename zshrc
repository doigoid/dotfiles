# Path to your oh-my-zsh configuration.
DOTFILES=$HOME/.dotfiles

# Path to your oh-my-zsh configuration.
ZSH=$DOTFILES/oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(launchctl git git-extras git-remote-branch github osx django pip python redis-cli knife)
source $ZSH/oh-my-zsh.sh

export GEM_HOME=~/.gem
export GEM_PATH=~/.gem:$GEM_PATH
export WORKON_HOME=~/envs

export PATH=/usr/local/share/python:$GEM_HOME/bin:/usr/local/bin:$PATH

source $DOTFILES/secrets

export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
source /usr/local/bin/virtualenvwrapper.sh

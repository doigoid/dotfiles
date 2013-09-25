# Path to your oh-my-zsh configuration.
DOTFILES=$HOME/.dotfiles

# Path to your oh-my-zsh configuration.
OHMYZSH=$DOTFILES/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

plugins=(launchctl git git-extras git-remote-branch github osx django pip python redis-cli knife)

source $OHMYZSH/oh-my-zsh.sh

export GEM_HOME=~/.gem
export GEM_PATH=~/.gem:$GEM_PATH
export WORKON_HOME=~/envs
source /usr/local/bin/virtualenvwrapper.sh

export PATH=/usr/local/share/python:$GEM_HOME/bin:/usr/local/bin:$PATH

source $DOTFILES/secrets
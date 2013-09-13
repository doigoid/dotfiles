# Path to your oh-my-zsh configuration.
ZSH=$HOME/.dotfiles/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

plugins=(git git-extras git-remote-branch github osx django pip python redis-cli knife)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/local/bin:$PATH
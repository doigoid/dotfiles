# Path to your oh-my-zsh configuration.
DOTFILES=$HOME/.dotfiles

# Path to your oh-my-zsh configuration.
ZSH=$DOTFILES/oh-my-zsh

ZSH_THEME="robbyrussell"
plugins=(launchctl git git-extras git-remote-branch github osx django pip python redis-cli)
source $ZSH/oh-my-zsh.sh

export EDITOR=emacs

export PATH=/usr/local/share/python:/usr/local/bin:/usr/local/sbin:$HOME/.bin:$GEM_HOME/bin:$PATH:/usr/local/Cellar/node/7.10.0/bin
source $DOTFILES/secrets

PATH=$PATH:$HOME/.rvm/bin:$HOME/.gems/bin:$HOME/bin;

[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

# has_virtualenv() {
#     if [ -e .venv ]; then
#         source `cat .venv`
#     fi
# }
# venv_cd () {
#     cd "$@" && has_virtualenv
# }
# alias cd="venv_cd"

source $DOTFILES/localrc

export WORKON_HOME=$HOME/.envs
export VIRTUALENVWRAPPER_PYTHON=/usr/local/bin/python
source /usr/local/bin/virtualenvwrapper.sh

eval "$(direnv hook zsh)"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"

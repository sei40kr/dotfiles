# custom_env.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

## Java

# SDKMAN!

if [[ -n "$SDKMAN_DIR" ]]; then
  . "${SDKMAN_DIR}/bin/sdkman-init.sh"
fi


## Python

alias pyfind="bfs . -name '*.py' | fzf"


## Ruby

alias rbfind="bfs . -name '*.rb' | fzf"

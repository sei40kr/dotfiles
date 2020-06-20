# aliases.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

alias d=dirs
alias po=popd
alias pu=pushd
alias u='cd ..'
alias cx='chmod +x'

# brew
if is_macos; then
    alias brew='HOMEBREW_GITHUB_API_TOKEN="$HOMEBREW_GITHUB_API_TOKEN" brew'

    alias brewp='brew pin'
    alias brews='brew list -1'
    alias brewsp='brew list --pinned'
    alias bubo='brew update && brew outdated'
    alias bubc='brew upgrade && brew cleanup'
    alias bubu='bubo && bubc'
    alias brewc='brew cleanup'
    alias bsl='brew services list'
    alias bsr='brew services run'
    alias bson='brew services start'
    alias bsoff='brew services stop'
fi

# cordova
alias co='cordova'
alias cob='cordova build'
alias cor='cordova run'
alias coc='cordova clean'
alias cop='cordova platform list'
alias copa='cordova platform add'
alias copr='cordova platform remove'
alias copl='cordova plugin list'
alias copla='cordova plugin add'
alias coplr='cordova plugin remove'

# coreutils
alias md='mkdir -p'
alias rd=rmdir
alias sortnr='sort -nr'

# fzf
alias preview="fzf --preview 'bat --color always {}'"

# gdb
alias gdb='gdb -ex start'

# lazydocker
alias lzd=lazydocker

# ncdu
alias du='ncdu --color dark -rr -x --exclude .git --exclude node_modules'

## Python

# pip
alias pip='noglob pip'

# ptpython
alias ptpython='PYTHONSTARTUP="${XDG_CONFIG_HOME:-${HOME}/.config}/ptpython/pythonstartup.py" ptpython'

# rbenv
alias rubies='rbenv versions'
alias gemsets='rbenv gemset list'

# rg
alias notes="rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"
alias pygrep="rg --iglob '**/*.py'"

# ruby
alias rb='ruby'

# ssh
alias ssh='env TERM=xterm-256color ssh'

# alias_def.zsh
# author: Seong Yong-ju <sei40kr@gmail.com>

alias d=dirs
alias po=popd
alias pu=pushd
alias u='cd ..'
alias cx='chmod +x'

# bat
alias cat='bat --theme TwoDark'

# brew
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

# bundle
alias be="bundle exec"
alias bl="bundle list"
alias bp="bundle package"
alias bo="bundle open"
alias bout="bundle outdated"
alias bu="bundle update"
alias bi="bundle_install"
alias bcn="bundle clean"

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

# docker
alias di='docker info'
alias dlg='docker container logs'
alias dls='docker container ls'
alias dlsa='docker container ls -a'
alias dr='docker container run'
alias dt='docker top'
alias dv='docker version'
alias dpo='docker container port'
alias dpu='docker pull'
alias dx='docker container exec'
alias dbl='docker build'
alias dhh='docker help'
alias dpsa='docker container ps -a'
alias dils='docker image ls'
alias dit='docker image tag'
alias dip='docker image push'
alias dib='docker image build'
alias dnls='docker network ls'
alias dnc='docker network create'
alias dncn='docker network connect'
alias dndcn='docker network disconnect'
alias dnrm='docker network rm'
alias dvls='docker volume ls'
alias dvclean='docker volume rm $(docker volume ls -qf dangling=true)'
# remove docker intermediate images
alias drmi='docker rmi -f $(docker images -aq --filter dangling=true) 2>/dev/null'
# wipe everything, kill all running processes, remove all containers and images
alias dwipe='docker kill $(docker ps -q) 2>/dev/null;docker rm $(docker ps -aq) 2>/dev/null;docker rmi -f $(docker images -aq) 2>/dev/null'

# exa
alias ls='exa -F'
alias la='exa -laFh'
alias tree='exa -T'

# fzf
alias preview="fzf --preview 'bat --color always {}'"

# gdb
alias gdb='gdb -ex start'

# lazydocker
alias lzd=lazydocker

# ncdu
alias du='ncdu --color dark -rr -x --exclude .git --exclude node_modules'

# npm
alias npmg='npm i -g '
alias npmS='npm i -S '
alias npmD='npm i -D '
alias npmE='PATH="$(npm bin):${PATH}"'
alias npmO='npm outdated'
alias npmV='npm -v'
alias npmL='npm list'
alias npmL0='npm ls --depth=0'
alias npmst='npm start'
alias npmt='npm test'
alias npmR='npm run'
alias npmP='npm publish'
alias npmI='npm init'

## Python

# pip
alias pip='noglob pip'

# ptpython
alias ptpython='PYTHONSTARTUP="${XDG_CONFIG_HOME:-${HOME}/.config}/ptpython/pythonstartup.py" ptpython'

# prettyping
alias ping='prettyping --nolegend'

# rbenv
alias rubies='rbenv versions'
alias gemsets='rbenv gemset list'

# rg
alias notes="rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"
alias pygrep="rg --iglob '**/*.py'"

# ruby
alias rb='ruby'

# R
alias R='R --no-save --no-restore-data -q'

# ssh
alias ssh='env TERM=xterm-256color ssh'

# top
alias top='htop'

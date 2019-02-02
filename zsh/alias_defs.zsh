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
alias bubo='brew update; and brew outdated'
alias bubc='brew upgrade; and brew cleanup'
alias bubu='bubo; and bubc'
alias brewc='brew cleanup'

# coreutils
alias md='mkdir -p'
alias rd=rmdir
alias sortnr='sort -nr'

# cd-gitroot
alias U=cd-gitroot

# diff
alias diff='diff-so-fancy'

# emacs
alias edebug='command emacs --debug-init'

# exa
alias ls='exa -F'
alias la='exa -laFh'
alias tree='exa -T'

# fzf
alias preview="fzf --preview 'bat --color always {}'"

# ncdu
alias du='ncdu --color dark -rr -x --exclude .git --exclude node_modules'

# prettyping
alias ping='prettyping --nolegend'

# rbenv
alias rubies='rbenv versions'
alias gemsets='rbenv gemset list'

# rg
alias notes="rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"

# ssh
alias ssh='env TERM=xterm-256color ssh'

# top
alias top='htop'

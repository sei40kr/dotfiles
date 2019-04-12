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

# coreutils
alias md='mkdir -p'
alias rd=rmdir
alias sortnr='sort -nr'

# cd-gitroot
alias U=cd-gitroot

# diff
alias diff='diff-so-fancy'

# docker
alias di='docker info'
alias dlg='docker container logs'
alias dls='docker container ls'
alias dlsa='docker container ls -a'
alias dr='docker container run'
alias drm='docker container rm'
alias drmf='docker container rm -f'
alias dst='docker container start'
alias dstp='docker container stop'
alias dt='docker top'
alias dv='docker version'
alias dpo='docker container port'
alias dpu='docker pull'
alias dx='docker container exec'
alias dbl='docker build'
alias dhh='docker help'
alias dcin='docker container inspect'
alias dpsa='docker container ps -a'
alias dirm='docker image rm'
alias dils='docker image ls'
alias dit='docker image tag'
alias dip='docker image push'
alias dib='docker image build'
alias dii='docker image inspect'
alias dnls='docker network ls'
alias dni='docker network inspect'
alias dnc='docker network create'
alias dncn='docker network connect'
alias dndcn='docker network disconnect'
alias dnrm='docker network rm'
alias dvls='docker volume ls'
alias dvi='docker volume inspect'
alias dvclean='docker volume rm $(docker volume ls -qf dangling=true)'
# remove docker intermediate images
alias drmi='docker rmi -f $(docker images -aq --filter dangling=true) 2>/dev/null'
# wipe everything, kill all running processes, remove all containers and images
alias dwipe='docker kill $(docker ps -q) 2>/dev/null;docker rm $(docker ps -aq) 2>/dev/null;docker rmi -f $(docker images -aq) 2>/dev/null'

# emacs
alias emacs='_emacsfun'
alias e='_emacsfun'
alias te='_emacsfun -t'
alias eeval='_emacsfun -e'
if displays_graphic; then
    alias edebug='command emacs --debug-init'
else
    alias edebug='command emacs --debug-init -nw'
fi

# exa
alias ls='exa -F'
alias la='exa -laFh'
alias tree='exa -T'

# fzf
alias preview="fzf --preview 'bat --color always {}'"

# kubectl
alias k=kubectl
alias kaf='kubectl apply -f'
alias keti='kubectl exec -ti'
alias kcuc='kubectl config use-context'
alias kcsc='kubectl config set-context'
alias kcdc='kubectl config delete-context'
alias kccc='kubectl config current-context'
alias kdel='kubectl delete'
alias kdelf='kubectl delete -f'
alias kgp='kubectl get pods'
alias kgpw='kgp --watch'
alias kgpwide='kgp -o wide'
alias kep='kubectl edit pods'
alias kdp='kubectl describe pods'
alias kdelp='kubectl delete pods'
alias kgpl='kgp -l'
alias kgs='kubectl get svc'
alias kgsw='kgs --watch'
alias kgswide='kgs -o wide'
alias kes='kubectl edit svc'
alias kds='kubectl describe svc'
alias kdels='kubectl delete svc'
alias kgi='kubectl get ingress'
alias kei='kubectl edit ingress'
alias kdi='kubectl describe ingress'
alias kdeli='kubectl delete ingress'
alias kgns='kubectl get namespaces'
alias kens='kubectl edit namespace'
alias kdns='kubectl describe namespace'
alias kdelns='kubectl delete namespace'
alias kcn='kubectl config set-context --current --namespace'
alias kgcm='kubectl get configmaps'
alias kecm='kubectl edit configmap'
alias kdcm='kubectl describe configmap'
alias kdelcm='kubectl delete configmap'
alias kgsec='kubectl get secret'
alias kdsec='kubectl describe secret'
alias kdelsec='kubectl delete secret'
alias kgd='kubectl get deployment'
alias kgdw='kgd --watch'
alias kgdwide='kgd -o wide'
alias ked='kubectl edit deployment'
alias kdd='kubectl describe deployment'
alias kdeld='kubectl delete deployment'
alias ksd='kubectl scale deployment'
alias kgcj='kubectl get cronjob'
alias kecj='kubectl edit cronjob'
alias kdcj='kubectl describe cronjob'
alias kdelcj='kubectl delete cronjob'
alias krsd='kubectl rollout status deployment'
alias kgrs='kubectl get rs'
alias krh='kubectl rollout history'
alias kru='kubectl rollout undo'
alias kpf="kubectl port-forward"
alias kga='kubectl get all'
alias kgaa='kubectl get all --all-namespaces'
alias kl='kubectl logs'
alias klf='kubectl logs -f'
alias kcp='kubectl cp'
alias kgno='kubectl get nodes'
alias keno='kubectl edit node'
alias kdno='kubectl describe node'
alias kdelno='kubectl delete node'

# ncdu
alias du='ncdu --color dark -rr -x --exclude .git --exclude node_modules'

# npm
alias npmg="npm i -g "
alias npmS="npm i -S "
alias npmD="npm i -D "
alias npmE='PATH="$(npm bin)":"$PATH"'
alias npmO="npm outdated"
alias npmV="npm -v"
alias npmL="npm list"
alias npmL0="npm ls --depth=0"
alias npmst="npm start"
alias npmt="npm test"
alias npmR="npm run"# Run npm scripts
alias npmP="npm publish"# Run npm publish
alias npmI="npm init"# Run npm init

# prettyping
alias ping='prettyping --nolegend'

# rbenv
alias rubies='rbenv versions'
alias gemsets='rbenv gemset list'

# rg
alias notes="rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"

# ssh
alias ssh='env TERM=xterm-256color ssh'

# tig
alias tig='env TERM=screen-256color tig'

# top
alias top='htop'

# yarn
alias yd='yarn dev'
alias ytc='yarn test --coverage'

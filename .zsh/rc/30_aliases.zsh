#!/usr/bin/env zsh

# 30_aliases.zsh
# author: Seong Yong-ju ( @sei40kr )

# brew {{{
alias brews='brew list -1'
alias bubo='brew update && brew outdated'
alias bubc='brew upgrade && brew cleanup'
alias bubu='bubo && bubc'
# }}}

# bundler {{{
alias be='bundle exec'
alias bl='bundle list'
alias bp='bundle package'
alias bo='bundle open'
alias bout='bundle outdated'
alias bu='bundle update'
alias bi='bundle_install'
alias bcn='bundle clean'
# }}}

# common-aliases {{{
alias zshrc='$EDITOR ~/.zshrc'

alias t='tail -f'

alias dud='du -d 1 -h'
alias duf='du -sh *'
alias fd='bfs . -type d -name'
alias ff='bfs . -type f -name'

alias h='history'
alias help='man'
alias p='ps -f'
alias sortnr='sort -n -r'
alias unexport='unset'
# }}}

# directories {{{
alias u='builtin cd ..'

alias md='mkdir -p'
alias rd='rmdir'
# }}}

# docker-compose {{{
alias dco='docker-compose'

alias dcb='docker-compose build'
alias dce='docker-compose exec'
alias dcps='docker-compose ps'
alias dcrestart='docker-compose restart'
alias dcrm='docker-compose rm'
alias dcr='docker-compose run'
alias dcstop='docker-compose stop'
alias dcup='docker-compose up'
alias dcdn='docker-compose down'
alias dcl='docker-compose logs'
alias dclf='docker-compose logs -f'
# }}}

# gem {{{
alias gemb='gem build *.gemspec'
alias gemp='gem push *.gem'
# }}}

# git {{{
gcd='git checkout dev'
glud='git pull upstream dev'
gmod='git merge origin/dev'
gmud='git merge upstream/dev'
grbd='git rebase dev'
# }}}

# golang {{{
alias gob='go build'
alias goc='go clean'
alias god='go doc'
alias gof='go fmt'
alias gofa='go fmt . ./...'
alias gog='go get'
alias goi='go install'
alias gol='go list'
alias gor='go run'
alias got='go test'
alias gov='go vet'
# }}}

# jonas/tig {{{
alias tig='env TERM="screen-256color-italic" tig'

alias tis='tig status'
alias til='tig log'
alias tib='tig blame -C'
# }}}

# mvn {{{
alias mvncie='mvn clean install eclipse:eclipse'
alias mvnci='mvn clean install'
alias mvncist='mvn clean install -DskipTests'
alias mvncisto='mvn clean install -DskipTests --offline'
alias mvne='mvn eclipse:eclipse'
alias mvnce='mvn clean eclipse:clean eclipse:eclipse'
alias mvncv='mvn clean verify'
alias mvnd='mvn deploy'
alias mvnp='mvn package'
alias mvnc='mvn clean'
alias mvncom='mvn compile'
alias mvnct='mvn clean test'
alias mvnt='mvn test'
alias mvnag='mvn archetype:generate'
alias mvn-updates='mvn versions:display-dependency-updates'
alias mvntc7='mvn tomcat7:run'
alias mvntc='mvn tomcat:run'
alias mvnjetty='mvn jetty:run'
alias mvndt='mvn dependency:tree'
alias mvns='mvn site'
alias mvnsrc='mvn dependency:sources'
alias mvndocs='mvn dependency:resolve -Dclassifier=javadoc'
# }}}

# npm {{{
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
alias npmR="npm run"
alias npmP="npm publish"
# }}}

# nvim {{{
alias vim='nvim'
# }}}

# perl {{{
alias pbi='perlbrew install'
alias pbl='perlbrew list'
alias pbo='perlbrew off'
alias pbs='perlbrew switch'
alias pbu='perlbrew use'

alias pd='perldoc'

alias ple='perl -wlne'
# }}}

# python {{{
alias pyfind='bfs . -name "*.py"'
# }}}

# postgres {{{
alias startpost='pg_ctl -D /usr/local/var/postgres -l /usr/local/var/postgres/server.log start'
alias stoppost='pg_ctl -D /usr/local/var/postgres stop -s -m fast'
alias restartpost='stoppost && sleep 1 && startpost'
alias reloadpost='pg_ctl reload -D /usr/local/var/postgres -s'
alias statuspost='pg_ctl status -D /usr/local/var/postgres -s'
# }}}

# rails {{{
alias devlog='tail -f log/development.log'
alias prodlog='tail -f log/production.log'
alias testlog='tail -f log/test.log'

alias -g RED='RAILS_ENV=development'
alias -g REP='RAILS_ENV=production'
alias -g RET='RAILS_ENV=test'

alias rc='rails console'
alias rcs='rails console --sandbox'
alias rd='rails destroy'
alias rdb='rails dbconsole'
alias rgm='rails generate migration'
alias rp='rails plugin'
alias ru='rails runner'
alias rs='rails server'
alias rsd='rails server --debugger'
alias rsp='rails server --port'

alias rdm='rake db:migrate'
alias rdms='rake db:migrate:status'
alias rdr='rake db:rollback'
alias rdc='rake db:create'
alias rds='rake db:seed'
alias rdd='rake db:drop'
alias rdrs='rake db:reset'
alias rdtc='rake db:test:clone'
alias rdtp='rake db:test:prepare'
alias rdmtc='rake db:migrate db:test:clone'
alias rdsl='rake db:schema:load'
alias rlc='rake log:clear'
alias rn='rake notes'
alias rr='rake routes'
alias rt='rake test'
alias rmd='rake middleware'
alias rsts='rake stats'
# }}}

# rake {{{
alias rake='noglob rake'
alias brake='noglob bundle exec rake'
alias srake='noglob sudo rake'
alias sbrake='noglob sudo bundle exec rake'
# }}}

# react-native {{{
alias rn='react-native'
alias rns='react-native start'
alias rnlink='react-native link'

alias rnand='react-native run-android'
alias rnios='react-native run-ios'
alias rnios4s='react-native run-ios --simulator "iPhone 4s"'
alias rnios5='react-native run-ios --simulator "iPhone 5"'
alias rnios5s='react-native run-ios --simulator "iPhone 5s"'
alias rnios6='react-native run-ios --simulator "iPhone 6"'
alias rnios6s='react-native run-ios --simulator "iPhone 6s"'

alias rnland='react-native log-android'
alias rnlios='react-native log-ios'
# }}}

# rsync {{{
alias rsync-copy="rsync -avz --progress -h"
alias rsync-move="rsync -avz --progress -h --remove-source-files"
alias rsync-update="rsync -avzu --progress -h"
alias rsync-synchronize="rsync -avzu --delete --progress -h"
# }}}

# yarn {{{
alias y="yarn "
alias ya="yarn add"
alias ycc="yarn cache clean"
alias yh="yarn help"
alias yo="yarn outdated"
alias yui="yarn upgrade-interactive"
# }}}

# zmv {{{
alias zmv='noglob zmv'
# }}}

# mollifier/anyframe {{{
alias U='cd-gitroot'
# }}}

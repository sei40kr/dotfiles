if [ -z "$_fish_abbreviations_initialized" ]
    set -U _fish_abbreviations_initialized 1

    # directory
    abbr -a d dirs

    # docker

    abbr -a dk docker
    abbr -a dka docker attach
    abbr -a dkb docker build
    abbr -a dkd docker diff
    abbr -a dkdf docker system df
    abbr -a dke docker exec
    abbr -a dkE docker exec -it
    abbr -a dkh docker history
    abbr -a dki docker images
    abbr -a dkin docker inspect
    abbr -a dkim docker import
    abbr -a dkk docker kill
    abbr -a dkl docker logs
    abbr -a dkli docker login
    abbr -a dklo docker logout
    abbr -a dkls docker ps
    abbr -a dkp docker pause
    abbr -a dkP docker unpause
    abbr -a dkpl docker pull
    abbr -a dkph docker push
    abbr -a dkps docker ps
    abbr -a dkpsa docker ps -a
    abbr -a dkr docker run
    abbr -a dkR docker run -it --rm
    abbr -a dkRe docker run -it --rm --entrypoint /bin/bash
    abbr -a dkRM docker system prune
    abbr -a dkrm docker rm
    abbr -a dkrmi docker rmi
    abbr -a dkrn docker rename
    abbr -a dks docker start
    abbr -a dkS docker restart
    abbr -a dkss docker stats
    abbr -a dksv docker save
    abbr -a dkt docker tag
    abbr -a dktop docker top
    abbr -a dkup docker update
    abbr -a dkV docker volume
    abbr -a dkv docker version
    abbr -a dkw docker wait
    abbr -a dkx docker stop

    abbr -a dkC docker container
    abbr -a dkCa docker container attach
    abbr -a dkCcp docker container cp
    abbr -a dkCd docker container diff
    abbr -a dkCe docker container exec
    abbr -a dkCin docker container inspect
    abbr -a dkCk docker container kill
    abbr -a dkCl docker container logs
    abbr -a dkCls docker container ls
    abbr -a dkCp docker container pause
    abbr -a dkCpr docker container prune
    abbr -a dkCrn docker container rename
    abbr -a dkCS docker container restart
    abbr -a dkCrm docker container rm
    abbr -a dkCr docker container run
    abbr -a dkCR docker container run -it --rm
    abbr -a dkCRe docker container run -it --rm --entrypoint /bin/bash
    abbr -a dkCs docker container start
    abbr -a dkCss docker container stats
    abbr -a dkCx docker container stop
    abbr -a dkCtop docker container top
    abbr -a dkCP docker container unpause
    abbr -a dkCup docker container update
    abbr -a dkCw docker container wait

    abbr -a dkI docker image
    abbr -a dkIb docker image build
    abbr -a dkIh docker image history
    abbr -a dkIim docker image import
    abbr -a dkIin docker image inspect
    abbr -a dkIls docker image ls
    abbr -a dkIpr docker image prune
    abbr -a dkIpl docker image pull
    abbr -a dkIph docker image push
    abbr -a dkIrm docker image rm
    abbr -a dkIsv docker image save
    abbr -a dkIt docker image tag

    abbr -a dkV docker volume
    abbr -a dkVin docker volume inspect
    abbr -a dkVls docker volume ls
    abbr -a dkVpr docker volume prune
    abbr -a dkVrm docker volume rm

    abbr -a dkN docker network
    abbr -a dkNs docker network connect
    abbr -a dkNx docker network disconnect
    abbr -a dkNin docker network inspect
    abbr -a dkNls docker network ls
    abbr -a dkNpr docker network prune
    abbr -a dkNrm docker network rm

    abbr -a dkY docker system
    abbr -a dkYdf docker system df
    abbr -a dkYpr docker system prune

    abbr -a dkK docker stack
    abbr -a dkKls docker stack ls
    abbr -a dkKps docker stack ps
    abbr -a dkKrm docker stack rm

    abbr -a dkW docker swarm

    abbr -a dkrmC docker rm \(docker ps -qaf status=exited\)
    abbr -a dkrmI docker rmi \(docker images -qf dangling=true\)
    abbr -a dkrmV docker volume rm \(docker volume ls -qf dangling=true\)

    abbr -a dkc docker-compose
    abbr -a dkcb docker-compose build
    abbr -a dkcB docker-compose build --no-cache
    abbr -a dkcd docker-compose down
    abbr -a dkce docker-compose exec
    abbr -a dkck docker-compose kill
    abbr -a dkcl docker-compose logs
    abbr -a dkcls docker-compose ps
    abbr -a dkcp docker-compose pause
    abbr -a dkcP docker-compose unpause
    abbr -a dkcpl docker-compose pull
    abbr -a dkcph docker-compose push
    abbr -a dkcps docker-compose ps
    abbr -a dkcr docker-compose run
    abbr -a dkcR docker-compose run --rm
    abbr -a dkcrm docker-compose rm
    abbr -a dkcs docker-compose start
    abbr -a dkcsc docker-compose scale
    abbr -a dkcS docker-compose restart
    abbr -a dkcu docker-compose up
    abbr -a dkcU docker-compose up -d
    abbr -a dkcv docker-compose version
    abbr -a dkcx docker-compose stop


    # emacs

    abbr -a te et


    # extract

    abbr -a x extract


    # gem

    abbr -a gemb gem build \*.buildspec
    abbr -a gemp gem build \*.gem


    # git

    abbr -a g git

    abbr -a gb git branch
    abbr -a gba git branch --all --verbose
    abbr -a gbc git checkout -b
    abbr -a gbd git branch --delete
    abbr -a gbD git branch --delete --force
    abbr -a gbl git branch --verbose
    abbr -a gbL git branch --all --verbose
    abbr -a gbm git branch --move
    abbr -a gbM git branch --move --force
    abbr -a gbr git branch --move
    abbr -a gbR git branch --move --force
    abbr -a gbs git show-branch
    abbr -a gbS git show-branch --all
    abbr -a gbv git branch --verbose
    abbr -a gbV git branch --verbose --verbose
    abbr -a gbx git branch --delete
    abbr -a gbX git branch --delete --force

    abbr -a gc git commit --verbose
    abbr -a gca git commit --verbose --all
    abbr -a gcm git commit --message
    abbr -a gcS git commit -S --verbose
    abbr -a gcSa git commit -S --verbose --all
    abbr -a gcSm git commit -S --message
    abbr -a gcam git commit --all --message
    abbr -a gco git checkout
    abbr -a gcO git checkout --patch
    abbr -a gcf git commit --amend --reuse-message HEAD
    abbr -a gcSf git commit -S --amend --reuse-message HEAD
    abbr -a gcF git commit --verbose --amend
    abbr -a gcSF git commit -S --verbose --amend
    abbr -a gcp git cherry-pick --ff
    abbr -a gcP git cherry-pick --no-commit
    abbr -a gcr git revert
    abbr -a gcR git reset HEAD^
    abbr -a gcs git show
    abbr -a gcl git-commit-lost
    abbr -a gcy git cherry -v --abbrev
    abbr -a gcY git cherry -v

    abbr -a gCl git --no-pager diff --name-only --diff-filter=U
    abbr -a gCa git add \(git --no-pager diff --name-only --diff-filter=U\)
    abbr -a gCe git mergetool \(git --no-pager diff --name-only --diff-filter=U\)
    abbr -a gCo git checkout --ours --
    abbr -a gCO git checkout --ours -- \(git --no-pager diff --name-only --diff-filter=U\)
    abbr -a gCt git checkout --theirs --
    abbr -a gCT git checkout --theirs -- \(git --no-pager diff --name-only --diff-filter=U\)

    abbr -a gd git ls-files
    abbr -a gdc git ls-files --cached
    abbr -a gdx git ls-files --deleted
    abbr -a gdm git ls-files --modified
    abbr -a gdu git ls-files --other --exclude-standard
    abbr -a gdk git ls-files --killed
    abbr -a gdi git status --porcelain --short --ignored \| sed -n 's/^!! //p'

    abbr -a gf git fetch
    abbr -a gfa git fetch --all
    abbr -a gfc git clone
    abbr -a gfcr git clone --recurse-submodules
    abbr -a gfm git pull
    abbr -a gfr git pull --rebase

    abbr -a gFi git flow init
    abbr -a gFf git flow feature
    abbr -a gFb git flow bugfix
    abbr -a gFl git flow release
    abbr -a gFh git flow hotfix
    abbr -a gFs git flow support
    abbr -a gFfl git flow feature list
    abbr -a gFfs git flow feature start
    abbr -a gFff git flow feature finish
    abbr -a gFfp git flow feature publish
    abbr -a gFft git flow feature track
    abbr -a gFfd git flow feature diff
    abbr -a gFfr git flow feature rebase
    abbr -a gFfc git flow feature checkout
    abbr -a gFfm git flow feature pull
    abbr -a gFfx git flow feature delete
    abbr -a gFbl git flow bugfix list
    abbr -a gFbs git flow bugfix start
    abbr -a gFbf git flow bugfix finish
    abbr -a gFbp git flow bugfix publish
    abbr -a gFbt git flow bugfix track
    abbr -a gFbd git flow bugfix diff
    abbr -a gFbr git flow bugfix rebase
    abbr -a gFbc git flow bugfix checkout
    abbr -a gFbm git flow bugfix pull
    abbr -a gFbx git flow bugfix delete
    abbr -a gFll git flow release list
    abbr -a gFls git flow release start
    abbr -a gFlf git flow release finish
    abbr -a gFlp git flow release publish
    abbr -a gFlt git flow release track
    abbr -a gFld git flow release diff
    abbr -a gFlr git flow release rebase
    abbr -a gFlc git flow release checkout
    abbr -a gFlm git flow release pull
    abbr -a gFlx git flow release delete
    abbr -a gFhl git flow hotfix list
    abbr -a gFhs git flow hotfix start
    abbr -a gFhf git flow hotfix finish
    abbr -a gFhp git flow hotfix publish
    abbr -a gFht git flow hotfix track
    abbr -a gFhd git flow hotfix diff
    abbr -a gFhr git flow hotfix rebase
    abbr -a gFhc git flow hotfix checkout
    abbr -a gFhm git flow hotfix pull
    abbr -a gFhx git flow hotfix delete
    abbr -a gFsl git flow support list
    abbr -a gFss git flow support start
    abbr -a gFsf git flow support finish
    abbr -a gFsp git flow support publish
    abbr -a gFst git flow support track
    abbr -a gFsd git flow support diff
    abbr -a gFsr git flow support rebase
    abbr -a gFsc git flow support checkout
    abbr -a gFsm git flow support pull
    abbr -a gFsx git flow support delete

    abbr -a gg git grep
    abbr -a ggi git grep --ignore-case
    abbr -a ggl git grep --files-with-matches
    abbr -a ggL git grep --files-without-matches
    abbr -a ggv git grep --invert-match
    abbr -a ggw git grep --word-regexp

    abbr -a gia git add
    abbr -a giA git add --patch
    abbr -a giu git add --update
    abbr -a gid git diff --no-ext-diff --cached
    abbr -a giD git diff --no-ext-diff --cached --word-diff
    abbr -a gii git update-index --assume-unchanged
    abbr -a giI git update-index --no-assume-unchanged
    abbr -a gir git reset
    abbr -a giR git reset --patch
    abbr -a gix git rm -r --cached
    abbr -a giX git rm -rf --cached

    abbr -a glc git shortlog --summary --numbered

    abbr -a gm git merge
    abbr -a gmC git merge --no-commit
    abbr -a gmF git merge --no-ff
    abbr -a gma git merge --abort
    abbr -a gmt git mergetool

    abbr -a gp git push
    abbr -a gpf git push --force-with-lease
    abbr -a gpF git push --force
    abbr -a gpa git push --all
    abbr -a gpA git push --all and git push --tags
    abbr -a gpt git push --tags
    abbr -a gpc git push --set-upstream origin \(git-branch-current \^/dev/null\)
    abbr -a gpp git pull origin \(git-branch-current \^/dev/null\) and git push origin \(git-branch-current \^/dev/null\)

    abbr -a gr git rebase
    abbr -a gra git rebase --abort
    abbr -a grc git rebase --continue
    abbr -a gri git rebase --interactive
    abbr -a grs git rebase --skip

    abbr -a gR git remote
    abbr -a gRl git remote --verbose
    abbr -a gRa git remote add
    abbr -a gRx git remote rm
    abbr -a gRm git remote rename
    abbr -a gRu git remote update
    abbr -a gRp git remote prune
    abbr -a gRs git remote show
    abbr -a gRb git-hub-browse

    abbr -a gs git stash
    abbr -a gsa git stash apply
    abbr -a gsx git stash drop
    abbr -a gsX git-stash-clear-interactive
    abbr -a gsl git stash list
    abbr -a gsL git-stash-dropped
    abbr -a gsd git stash show --patch --stat
    abbr -a gsp git stash pop
    abbr -a gsr git-stash-recover
    abbr -a gss git stash save --include-untracked
    abbr -a gsS git stash save --patch --no-keep-index
    abbr -a gsw git stash save --include-untracked --keep-index

    abbr -a gS git submodule
    abbr -a gSa git submodule add
    abbr -a gSf git submodule foreach
    abbr -a gSi git submodule init
    abbr -a gSI git submodule update --init --recursive
    abbr -a gSl git submodule status
    abbr -a gSm git-submodule-move
    abbr -a gSs git submodule sync
    abbr -a gSu git submodule foreach git pull origin master
    abbr -a gSx git-submodule-remove

    abbr -a gt git tag
    abbr -a gtl git tag -l

    abbr -a gws git status --ignore-submodules=all --short
    abbr -a gwS git status --ignore-submodules=all
    abbr -a gwd git diff --no-ext-diff
    abbr -a gwD git diff --no-ext-diff --word-diff
    abbr -a gwr git reset --soft
    abbr -a gwR git reset --hard
    abbr -a gwc git clean -n
    abbr -a gwC git clean -f
    abbr -a gwx git rm -r
    abbr -a gwX git rm -rf


    # golang

    abbr -a gob go build
    abbr -a goc go clean
    abbr -a god go doc
    abbr -a gof go fmt
    abbr -a gog go get
    abbr -a goi go install
    abbr -a gol go list
    abbr -a gor go run
    abbr -a got go test
    abbr -a gov go vet


    # homebrew

    abbr -a brewc brew cleanup
    abbr -a brewC brew cleanup --force
    abbr -a brewi brew install
    abbr -a brewl brew list
    abbr -a brewo brew outdated
    abbr -a brews brew search
    abbr -a brewu brew update and brew upgrade
    abbr -a brewx brew remove

    abbr -a cask brew cask
    abbr -a caskc brew cask cleanup --outdated
    abbr -a caskC brew cask cleanup
    abbr -a caski brew cask install
    abbr -a caskl brew cask list
    abbr -a casko brew cask outdated
    abbr -a casks brew cask search
    abbr -a caskx brew cask uninstall


    # mvn

    abbr -a mvncie mvn clean install eclipse:eclipse
    abbr -a mvnci mvn clean install
    abbr -a mvncist mvn clean install -DskipTests
    abbr -a mvncisto mvn clean install -DskipTests --offline
    abbr -a mvne mvn eclipse:eclipse
    abbr -a mvnce mvn clean eclipse:clean eclipse:eclipse
    abbr -a mvncv mvn clean verify
    abbr -a mvnd mvn deploy
    abbr -a mvnp mvn package
    abbr -a mvnc mvn clean
    abbr -a mvncom mvn compile
    abbr -a mvnct mvn clean test
    abbr -a mvnt mvn test
    abbr -a mvnag mvn archetype:generate
    abbr -a mvn-updates mvn versions:display-dependency-updates
    abbr -a mvntc7 mvn tomcat7:run
    abbr -a mvntc mvn tomcat:run
    abbr -a mvnjetty mvn jetty:run
    abbr -a mvndt mvn dependency:tree
    abbr -a mvns mvn site
    abbr -a mvnsrc mvn dependency:sources
    abbr -a mvndocs mvn dependency:resolve -Dclassifier=javadoc


    # nmap

    abbr -a nmap_open_ports nmap --open
    abbr -a nmap_list_interfaces nmap --iflist
    abbr -a nmap_slow nmap -sS -v -T1
    abbr -a nmap_fin nmap -sF -v
    abbr -a nmap_full nmap -sS -T4 -PE -PP -PS80,443 -PY -g 53 -A -p1-65535 -v
    abbr -a nmap_check_for_firewall nmap -sA -p1-65535 -v -T4
    abbr -a nmap_ping_through_firewall nmap -PS -PA
    abbr -a nmap_fast nmap -F -T5 --version-light --top-ports 300
    abbr -a nmap_detect_versions nmap -sV -p1-65535 -O --osscan-guess -T4 -Pn
    abbr -a nmap_check_for_vulns nmap --script=vulscan
    abbr -a nmap_full_udp nmap -sS -sU -T4 -A -v -PE -PS22,25,80 -PA21,23,80,443,3389
    abbr -a nmap_traceroute nmap -sP -PE -PS22,25,80 -PA21,23,80,3389 -PU -PO --traceroute
    abbr -a nmap_full_with_scripts sudo nmap -sS -sU -T4 -A -v -PE -PP -PS21,22,23,25,80,113,31339 -PA80,113,443,10042 -PO --script all
    abbr -a nmap_web_safe_osscan sudo nmap -p 80,443 -O -v --osscan-guess --fuzzy


    # npm

    abbr -a npmg npm i -g
    abbr -a npmS npm i -S
    abbr -a npmD npm i -D
    abbr -a npmO npm outdated
    abbr -a npmV npm -v
    abbr -a npmL npm list
    abbr -a npmL0 npm ls --depth=0
    abbr -a npmst npm start
    abbr -a npmt npm test
    abbr -a npmR npm run
    abbr -a npmP npm publish
    abbr -a npmI npm init


    # python

    abbr -a py python
    abbr -a py2 python2
    abbr -a py3 python3
    abbr -a pyfind "find . -name '*.py'"
    abbr -a pygrep "grep --include='*.py'"


    # perl

    abbr -a pl perl
    abbr -a pld perldoc
    abbr -a ple perl -wlne


    # pylint

    abbr -a pylint-quick pylint --reports=n


    # rails

    abbr -a ror bundle exec rails
    abbr -a rorc bundle exec rails console
    abbr -a rordc bundle exec rails dbconsole
    abbr -a rordm bundle exec rake db:migrate
    abbr -a rordM bundle exec rake db:migrate db:test:clone
    abbr -a rordr bundle exec rake db:rollback
    abbr -a rorg bundle exec rails generate
    abbr -a rorl tail -f \(ruby-app-root\)/log/development.log
    abbr -a rorlc bundle exec rake log:clear
    abbr -a rorp bundle exec rails plugin
    abbr -a rorr bundle exec rails runner
    abbr -a rors bundle exec rails server
    abbr -a rorsd bundle exec rails server --debugger
    abbr -a rorx bundle exec rails destroy


    # rake

    abbr -a brake bundle exec rake


    # ruby

    abbr -a rb ruby
    abbr -a rfind "find . -name '*.rb' | xargs grep -n"

    abbr -a gin gem install
    abbr -a gun gem uninstall
    abbr -a gli gem list

    abbr -a rbb bundle
    abbr -a rbbc bundle clean
    abbr -a rbbe bundle exec
    abbr -a rbbi bundle install --path vendor/bundle
    abbr -a rbbl bundle list
    abbr -a rbbo bundle open
    abbr -a rbbp bundle package
    abbr -a rbbu bundle update


    # utility

    abbr -a _ sudo
    abbr -a b eval \$BROWSER

    abbr -a diffu diff --unified
    abbr -a mkdir mkdir -p
    abbr -a p eval \$PAGER
    abbr -a po popd
    abbr -a pu pushd
    abbr -a sa abbr -l \| grep -i
    abbr -a type type -a

    abbr -a rmi rm -i
    abbr -a mvi mv -i
    abbr -a cpi cp -i
    abbr -a lni ln -i

    abbr -a l ls -1A
    abbr -a ll ls -lh
    abbr -a lr ll -R
    abbr -a la ll -A
    abbr -a lm la \| eval \$PAGER
    abbr -a lx ll -XB
    abbr -a lk ll -Sr
    abbr -a lt ll -tr
    abbr -a lc lt -c
    abbr -a lu lt -u
    abbr -a sl ls

    abbr -a df df -kh
    abbr -a du du -kh

    abbr -a http-serve python3 -m http.server


    # toggl

    abbr -a tgs toggl stop


    # trizen

    abbr trconf trizen -C
    abbr trupg trizen -Syua
    abbr trsu trizen -Syua --noconfirm
    abbr trin trizen -S
    abbr trins trizen -U
    abbr trre trizen -R
    abbr trrem trizen -Rns
    abbr trrep trizen -Si
    abbr trreps trizen -Ss
    abbr trloc trizen -Qi
    abbr trlocs trizen -Qs
    abbr trlst trizen -Qe
    abbr trorph trizen -Qtd
    abbr trinsd trizen -S --asdeps
    abbr trmir trizen -Syy


    # yarn

    abbr -a y yarn
    abbr -a ya yarn add
    abbr -a ycc yarn cache clean
    abbr -a yh yarn help
    abbr -a yo yarn outdated
    abbr -a yui yarn upgrade-interactive
end

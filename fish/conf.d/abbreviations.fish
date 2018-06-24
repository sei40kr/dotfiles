# abbreviations.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ -z "$_fish_abbreviations_initialized" ]
    echo 'INFO: Initializing universal variables...'
    echo 'INFO: This may take several minutes. Please wait.'

    set -U _fish_abbreviations_initialized 1

    set -Ue fish_user_abbreviations

    set -l basepath (dirname (status --current-filename))/abbreviations

    # built-in
    abbr -a u cd ..

    abbr -a abbrupd fish_update_abbreviations

    source $basepath/archlinux.fish
    source $basepath/docker.fish
    source $basepath/git.fish
    source $basepath/homebrew.fish

    # directory
    abbr -a d dirs


    # emacs

    abbr -a te et


    # extract

    abbr -a x extract


    # gem

    abbr -a gemb gem build \*.buildspec
    abbr -a gemp gem build \*.gem


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


    # tmuxinator

    abbr -a txs tmuxinator start
    abbr -a txo tmuxinator open
    abbr -a txn tmuxinator new
    abbr -a txl tmuxinator list


    # toggl

    abbr -a tgs toggl stop


    # yarn

    abbr -a y yarn
    abbr -a ya yarn add
    abbr -a ycc yarn cache clean
    abbr -a yh yarn help
    abbr -a yo yarn outdated
    abbr -a yui yarn upgrade-interactive
end

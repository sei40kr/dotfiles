# abbreviations.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ -z "$__fish_user_abbreviations_added" ]
    echo 'INFO: Initializing universal variables...'
    echo 'INFO: This may take several minutes. Please wait.'

    set -U __fish_user_abbreviations_added 1
    # clear all abbreviations
    set -Ue fish_user_abbreviations

    set -l basepath (dirname (status --current-filename))/abbreviations

    # built-in

    abbr -a d dirs
    abbr -a po popd
    abbr -a pu pushd
    abbr -a u cd ..

    abbr -a fa fish_find_abbreviation

    abbr -a _ sudo
    abbr -a mkdir mkdir -p
    abbr -a df df -kh
    abbr -a du du -kh

    abbr -a abbrupd fish_update_abbreviations


    source $basepath/archlinux.fish
    source $basepath/docker.fish
    source $basepath/git.fish
    source $basepath/golang.fish
    source $basepath/homebrew.fish
    source $basepath/java.fish
    source $basepath/node.fish
    source $basepath/python.fish
    source $basepath/ruby.fish
    source $basepath/scala.fish


    # emacs

    abbr -a te et


    # extract

    abbr -a x extract


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


    # perl

    abbr -a pl perl
    abbr -a pld perldoc
    abbr -a ple perl -wlne


    # tmuxinator

    abbr -a txs tmuxinator start
    abbr -a txo tmuxinator open
    abbr -a txn tmuxinator new
    abbr -a txl tmuxinator list


    # toggl

    abbr -a tgs toggl stop
end

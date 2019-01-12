# alias_def_nmap.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

alias -s nmap_open_ports 'nmap --open'
alias -s nmap_list_interfaces 'nmap --iflist'
alias -s nmap_slow 'nmap -sS -v -T1'
alias -s nmap_fin 'nmap -sF -v'
alias -s nmap_full 'nmap -sS -T4 -PE -PP -PS80,443 -PY -g 53 -A -p1-65535 -v'
alias -s nmap_check_for_firewall 'nmap -sA -p1-65535 -v -T4'
alias -s nmap_ping_through_firewall 'nmap -PS -PA'
alias -s nmap_fast 'nmap -F -T5 --version-light --top-ports 300'
alias -s nmap_detect_versions 'nmap -sV -p1-65535 -O --osscan-guess -T4 -Pn'
alias -s nmap_check_for_vulns 'nmap --script=vulscan'
alias -s nmap_full_udp 'nmap -sS -sU -T4 -A -v -PE -PS22,25,80 -PA21,23,80,443,3389'
alias -s nmap_traceroute 'nmap -sP -PE -PS22,25,80 -PA21,23,80,3389 -PU -PO --traceroute'
alias -s nmap_full_with_scripts 'sudo nmap -sS -sU -T4 -A -v -PE -PP -PS21,22,23,25,80,113,31339 -PA80,113,443,10042 -PO --script all'
alias -s nmap_web_safe_osscan 'sudo nmap -p 80,443 -O -v --osscan-guess --fuzzy'

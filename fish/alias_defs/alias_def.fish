# alias_def.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

alias -s c clear
alias -s d dirs
alias -s po popd
alias -s pu pushd
alias -s u 'cd ..'
alias -s ll 'ls -al'
alias -s cx 'chmod +x'

# cd-gitroot
alias -s U 'cd-gitroot'

# coreutils
alias -s md 'mkdir -p'
alias -s rd rmdir
alias -s sortnr 'sort -nr'

# emacs
alias -s te et
alias -s edebug 'command emacs --debug-init'

# extract
alias -s x extract

# rg
alias -s notes "rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"

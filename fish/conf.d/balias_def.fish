# balias_def.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

balias c clear
balias d dirs
balias po popd
balias pu pushd
balias u 'cd ..'
balias ll 'ls -al'
balias cx 'chmod +x'

# coreutils
balias md 'mkdir -p'
balias rd rmdir
balias sortnr 'sort -nr'

# emacs
balias te et
balias edebug 'command emacs --debug-init'

# extract
balias x extract

# rg
balias notes "rg 'TODO|FIXME|HACK|OPTIMIZE|REVIEW'"

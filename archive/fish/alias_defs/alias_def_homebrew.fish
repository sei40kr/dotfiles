# alias_def_homebrew.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ (uname) = Darwin ]
    # Homebrew
    alias -s brewp 'brew pin'
    alias -s brews 'brew list -1'
    alias -s brewsp 'brew list --pinned'
    alias -s bubo 'brew update; and brew outdated'
    alias -s bubc 'brew upgrade; and brew cleanup'
    alias -s bubu 'bubo; and bubc'
    alias -s brewc 'brew cleanup'
end

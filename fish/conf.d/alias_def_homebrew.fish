# alias_def_homebrew.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ (uname) = Darwin ]
    # Homebrew
    alias brewp 'brew pin'
    alias brews 'brew list -1'
    alias brewsp 'brew list --pinned'
    alias bubo 'brew update; and brew outdated'
    alias bubc 'brew upgrade; and brew cleanup'
    alias bubu 'bubo; and bubc'
    alias brewc 'brew cleanup'
end

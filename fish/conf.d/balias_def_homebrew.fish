# balias_def_homebrew.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ (uname) = Darwin ]
    # Homebrew
    balias brewp 'brew pin'
    balias brews 'brew list -1'
    balias brewsp 'brew list --pinned'
    balias bubo 'brew update; and brew outdated'
    balias bubc 'brew upgrade; and brew cleanup'
    balias bubu 'bubo; and bubc'
    balias brewc 'brew cleanup'
end

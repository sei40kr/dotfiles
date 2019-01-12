# alias_def_arch.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ -f '/etc/arch-release' ]
    # pacman
    alias pacupg 'sudo pacman -Syu'
    alias pacin 'sudo pacman -S'
    alias pacins 'sudo pacman -U'
    alias pacre 'sudo pacman -R'
    alias pacrem 'sudo pacman -Rns'
    alias pacrep 'pacman -Si'
    alias pacreps 'pacman -Ss'
    alias pacloc 'pacman -Qi'
    alias paclocs 'pacman -Qs'
    alias pacinsd 'sudo pacman -S --asdeps'
    alias pacmir 'sudo pacman -Syy'
    alias paclsorphans 'sudo pacman -Qdt'
    alias pacrmorphans 'sudo pacman -Rs \(pacman -Qtdq\)'
    alias pacfileupg 'sudo pacman -Fy'
    alias pacfiles 'pacman -Fs'
    alias pacls 'pacman -Ql'
    alias pacown 'pacman -Qo'
    alias pacupd 'sudo pacman -Sy'

    # trizen
    alias trconf 'trizen -C'
    alias trupg 'trizen -Syua'
    alias trsu 'trizen -Syua --noconfirm'
    alias trin 'trizen -S'
    alias trins 'trizen -U'
    alias trre 'trizen -R'
    alias trrem 'trizen -Rns'
    alias trrep 'trizen -Si'
    alias trreps 'trizen -Ss'
    alias trloc 'trizen -Qi'
    alias trlocs 'trizen -Qs'
    alias trlst 'trizen -Qe'
    alias trorph 'trizen -Qtd'
    alias trinsd 'trizen -S --asdeps'
    alias trmir 'trizen -Syy'
end

# alias_def_arch.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ -f '/etc/arch-release' ]
    # pacman
    alias -s pacupg 'sudo pacman -Syu'
    alias -s pacin 'sudo pacman -S'
    alias -s pacins 'sudo pacman -U'
    alias -s pacre 'sudo pacman -R'
    alias -s pacrem 'sudo pacman -Rns'
    alias -s pacrep 'pacman -Si'
    alias -s pacreps 'pacman -Ss'
    alias -s pacloc 'pacman -Qi'
    alias -s paclocs 'pacman -Qs'
    alias -s pacinsd 'sudo pacman -S --asdeps'
    alias -s pacmir 'sudo pacman -Syy'
    alias -s paclsorphans 'sudo pacman -Qdt'
    alias -s pacrmorphans 'sudo pacman -Rs \(pacman -Qtdq\)'
    alias -s pacfileupg 'sudo pacman -Fy'
    alias -s pacfiles 'pacman -Fs'
    alias -s pacls 'pacman -Ql'
    alias -s pacown 'pacman -Qo'
    alias -s pacupd 'sudo pacman -Sy'

    # trizen
    alias -s trconf 'trizen -C'
    alias -s trupg 'trizen -Syua'
    alias -s trsu 'trizen -Syua --noconfirm'
    alias -s trin 'trizen -S'
    alias -s trins 'trizen -U'
    alias -s trre 'trizen -R'
    alias -s trrem 'trizen -Rns'
    alias -s trrep 'trizen -Si'
    alias -s trreps 'trizen -Ss'
    alias -s trloc 'trizen -Qi'
    alias -s trlocs 'trizen -Qs'
    alias -s trlst 'trizen -Qe'
    alias -s trorph 'trizen -Qtd'
    alias -s trinsd 'trizen -S --asdeps'
    alias -s trmir 'trizen -Syy'
end

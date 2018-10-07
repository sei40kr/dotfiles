# balias_def_arch.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

if [ -f '/etc/arch-release' ]
    # pacman
    balias pacupg 'sudo pacman -Syu'
    balias pacin 'sudo pacman -S'
    balias pacins 'sudo pacman -U'
    balias pacre 'sudo pacman -R'
    balias pacrem 'sudo pacman -Rns'
    balias pacrep 'pacman -Si'
    balias pacreps 'pacman -Ss'
    balias pacloc 'pacman -Qi'
    balias paclocs 'pacman -Qs'
    balias pacinsd 'sudo pacman -S --asdeps'
    balias pacmir 'sudo pacman -Syy'
    balias paclsorphans 'sudo pacman -Qdt'
    balias pacrmorphans 'sudo pacman -Rs \(pacman -Qtdq\)'
    balias pacfileupg 'sudo pacman -Fy'
    balias pacfiles 'pacman -Fs'
    balias pacls 'pacman -Ql'
    balias pacown 'pacman -Qo'
    balias pacupd 'pacman -Sy'

    # trizen
    balias trconf 'trizen -C'
    balias trupg 'trizen -Syua'
    balias trsu 'trizen -Syua --noconfirm'
    balias trin 'trizen -S'
    balias trins 'trizen -U'
    balias trre 'trizen -R'
    balias trrem 'trizen -Rns'
    balias trrep 'trizen -Si'
    balias trreps 'trizen -Ss'
    balias trloc 'trizen -Qi'
    balias trlocs 'trizen -Qs'
    balias trlst 'trizen -Qe'
    balias trorph 'trizen -Qtd'
    balias trinsd 'trizen -S --asdeps'
    balias trmir 'trizen -Syy'
end

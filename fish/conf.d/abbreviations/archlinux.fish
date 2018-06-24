# archlinux.fish
# author: Seong Yong-ju <sei40kr@gmail.com>

# pacman
abbr -a pacupg sudo pacman -Syu
abbr -a pacin sudo pacman -S
abbr -a pacins sudo pacman -U
abbr -a pacre sudo pacman -R
abbr -a pacrem sudo pacman -Rns
abbr -a pacrep pacman -Si
abbr -a pacreps pacman -Ss
abbr -a pacloc pacman -Qi
abbr -a paclocs pacman -Qs
abbr -a pacinsd sudo pacman -S --asdeps
abbr -a pacmir sudo pacman -Syy
abbr -a paclsorphans sudo pacman -Qdt
abbr -a pacrmorphans sudo pacman -Rs \(pacman -Qtdq\)
abbr -a pacfileupg sudo pacman -Fy
abbr -a pacfiles pacman -Fs
abbr -a pacls pacman -Ql
abbr -a pacown pacman -Qo
abbr -a pacupd pacman -Sy

# trizen
abbr trconf trizen -C
abbr trupg trizen -Syua
abbr trsu trizen -Syua --noconfirm
abbr trin trizen -S
abbr trins trizen -U
abbr trre trizen -R
abbr trrem trizen -Rns
abbr trrep trizen -Si
abbr trreps trizen -Ss
abbr trloc trizen -Qi
abbr trlocs trizen -Qs
abbr trlst trizen -Qe
abbr trorph trizen -Qtd
abbr trinsd trizen -S --asdeps
abbr trmir trizen -Syy

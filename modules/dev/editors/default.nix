{ lib, ... }:

with lib; {
  imports = [
    ./emacs.nix
    ./doom-emacs.nix
    ./fonts.nix
    ./idea.nix
    ./ideavim.nix
    ./neovim.nix
    ./tabnine.nix
    ./tools.nix
    ./vscodium.nix
  ];
}

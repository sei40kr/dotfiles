{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./emacs.nix
    ./fonts.nix
    ./idea.nix
    ./ideavim.nix
    ./neovim.nix
    ./tabnine.nix
    ./vscodium.nix
  ];
}

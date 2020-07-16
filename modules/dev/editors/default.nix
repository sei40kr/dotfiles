{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./emacs.nix
    ./fonts.nix
    ./idea.nix
    ./ideavim.nix
    ./tabnine.nix
    ./vim.nix
    ./vscodium.nix
  ];
}

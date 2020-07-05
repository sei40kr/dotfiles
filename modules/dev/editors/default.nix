{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./emacs.nix ./idea.nix ./vim.nix ./vscodium.nix ];
}

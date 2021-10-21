{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = mapModulesRec' (toString ./.) import;

  environment.variables = {
    DOTFILES = config.dotfiles.dir;
    DOTFILES_BIN = config.dotfiles.binDir;
  };

  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };

  environment.systemPackages = with pkgs; [ coreutils git gnumake vim ];
}

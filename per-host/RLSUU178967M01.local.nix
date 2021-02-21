{ lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ ../secrets/per-host/RLSUU178967M01.local.nix ];

  modules = {
    dev = {
      editors = {
        emacs.enable = true;
        doomEmacs.enable = true;
        neovim.enable = true;
      };
      java = {
        enable = true;
        javaPackages."corretto64-11.0.8.10.1" = pkgs.my.corretto_11;
      };
      javascript.enable = true;
      web.enable = true;
      tools = {
        awsCli.enable = true;
        git.enable = true;
        kaggle.enable = true;
      };
    };
    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autostart = true;
      };
      tools.atcoderTools.enable = true;
    };
  };
}

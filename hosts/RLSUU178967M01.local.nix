{ lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ ../secrets/hosts/RLSUU178967M01.local.nix ];

  modules = {
    dev = {
      aws-cli.enable = true;
      java = {
        enable = true;
        javaPackages."corretto64-11.0.8.10.1" = pkgs.my.corretto_11;
      };
      javascript.enable = true;
      web.enable = true;
    };

    editors = {
      emacs = {
        enable = true;
        doom.enable = true;
      };
      neovim.enable = true;
    };

    shell = {
      git.enable = true;
      zsh.enable = true;
      tmux = {
        enable = true;
        autostart = true;
      };
      kaggle.enable = true;
      tools.atcoderTools.enable = true;
    };
  };
}

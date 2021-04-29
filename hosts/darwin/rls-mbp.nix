{ lib, pkgs, ... }:

with lib;
with lib.my; {
  modules = {
    dev = {
      aws-cli.enable = true;
      java.enable = true;
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
      zsh.enable = true;
      tmux = {
        enable = true;
        autoRun.enable = true;
      };
      atcoder-tools.enable = true;
      git.enable = true;
      kaggle.enable = true;
    };
  };

  user = {
    name = "RLSUU178967";
    shell = pkgs.zsh;
    uid = 1000;
  };
}

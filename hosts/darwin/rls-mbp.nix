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
      ideavim.enable = true;
      neovim.enable = true;
    };
    shell = {
      zsh.enable = true;
      tmux = {
        enable = true;
        autoRun = true;
      };
      git.enable = true;
      kaggle.enable = true;
    };
  };

  modules.term.kitty.enable = true;

  modules.term.font = {
    package = pkgs.nerdfonts.override { fonts = [ "Iosevka" ]; };
    name = "Iosevka Nerd Font";
    size = 20;
  };

  modules.term.theme.active = "doom-one";

  user.name = "RLSUU178967";
}

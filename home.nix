{ config, lib, options, pkgs, ... }:

{
  nixpkgs.config.allowUnfree = true;

  modules = {
    desktop = {
      xmonad.enable = true;
      apps = {
        rofi.enable = true;
        thunar.enable = true;
      };
      term.alacritty.enable = true;
      input.fcitx.enable = true;
    };
    dev = {
      editors = {
        emacs.enable = true;
        idea.enable = true;
      };

      cc.enable = true;
      git.enable = true;
      python.enable = true;
    };
    shell = {
      tmux.enable = true;
      zsh.enable = true;
    };
  };

  imports = [ <modules> ];
}

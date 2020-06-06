{ config, lib, options, pkgs, ... }:

{
  modules = {
    desktop = {
      xmonad.enable = true;
      apps.rofi.enable = true;
      term.alacritty.enable = true;
      input.fcitx.enable = true;
    };
    dev = {
      editors.emacs.enable = true;

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

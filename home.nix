{ config, lib, options, pkgs, ... }:

{
  modules = {
    desktop = {
      xmonad.enable = true;
      term.alacritty.enable = true;
    };
    dev = {
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

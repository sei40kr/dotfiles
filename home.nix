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

      tools.git.enable = true;

      cc.enable = true;
      go.enable = true;
      groovy.enable = true;
      haskell.enable = true;
      java.enable = true;
      kotlin.enable = true;
      python.enable = true;
      r.enable = true;
      ruby.enable = true;
      rust.enable = true;
    };
    shell = {
      tmux.enable = true;
      zsh.enable = true;
    };
  };

  imports = [ <modules> ];
}

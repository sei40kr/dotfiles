{ config, lib, options, pkgs, ... }:

{
  modules = {
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

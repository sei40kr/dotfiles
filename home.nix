{ config, lib, options, pkgs, ... }:

{
  modules = {
    dev = {
      cc.enable = true;
      git.enable = true;
    };
    shell = { zsh.enable = true; };
  };

  imports = [ <modules> ];
}

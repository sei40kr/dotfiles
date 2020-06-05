{ config, lib, options, pkgs, ... }:

{
  modules.dev.cc.enable = true;
  modules.dev.git.enable = true;

  imports = [ <modules> ];
}

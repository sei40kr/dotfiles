{ config, lib, options, pkgs, ... }:

{
  modules.dev.git.enable = true;

  imports = [ <modules> ];
}

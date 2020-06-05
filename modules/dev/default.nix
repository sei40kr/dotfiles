{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./cc.nix ./git.nix ./python.nix ];
}

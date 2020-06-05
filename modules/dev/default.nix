{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./editors ./cc.nix ./git.nix ./python.nix ];
}

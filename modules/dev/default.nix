{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./git.nix ];
}

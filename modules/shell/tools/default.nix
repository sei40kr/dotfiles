{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./flexget.nix ];
}

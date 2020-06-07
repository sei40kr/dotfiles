{ config, lib, pkgs, ... }:

with lib; {
  imports = [ ./japanese.nix ];
}

{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./chromium.nix ];
}

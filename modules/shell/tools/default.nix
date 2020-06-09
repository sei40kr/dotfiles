{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./bat.nix ./exa.nix ./flexget.nix ];
}

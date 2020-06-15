{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./parole.nix ./ristretto.nix ];
}

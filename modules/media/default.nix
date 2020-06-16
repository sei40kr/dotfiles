{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./eog.nix ./parole.nix ./ristretto.nix ./totem.nix ];
}

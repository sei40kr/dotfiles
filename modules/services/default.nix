{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./docker.nix ./fstrim.nix ./tlp.nix ];
}

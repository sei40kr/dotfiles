{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./fstrim.nix ./tlp.nix ];
}

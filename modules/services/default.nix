{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./deluge.nix
    ./docker.nix
    ./fstrim.nix
    ./picom.nix
    ./psd.nix
    ./redshift.nix
    ./tlp.nix
  ];
}

{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./deluge.nix
    ./docker.nix
    ./flexget.nix
    ./fstrim.nix
    ./picom.nix
    ./psd.nix
    ./redshift.nix
    ./tlp.nix
  ];
}

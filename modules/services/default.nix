{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./deluge.nix
    ./docker.nix
    ./flexget.nix
    ./fstrim.nix
    ./jellyfin.nix
    ./picom.nix
    ./psd.nix
    ./rclone.nix
    ./redshift.nix
    ./sshd.nix
    ./tlp.nix
  ];
}

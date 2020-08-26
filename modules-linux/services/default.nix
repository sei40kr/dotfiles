{ lib, ... }:

with lib; {
  imports = [
    ./cupsd.nix
    ./deluge.nix
    ./docker.nix
    ./fstrim.nix
    ./picom.nix
    ./psd.nix
    ./redshift.nix
    ./sshd.nix
    ./tlp.nix
  ];
}

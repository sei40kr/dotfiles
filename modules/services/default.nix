{ ... }:

{
  imports = [
    ./docker.nix
    ./expressvpn.nix
    ./flexget.nix
    ./google-drive.nix
    ./jellyfin.nix
    ./k8s.nix
    ./ssh.nix
    ./wireguard.nix
  ];
}

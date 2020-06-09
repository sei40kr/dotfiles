{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./bat.nix
    ./exa.nix
    ./flexget.nix
    ./htop.nix
    ./sshd.nix
    ./strace.nix
    ./tcpdump.nix
  ];
}

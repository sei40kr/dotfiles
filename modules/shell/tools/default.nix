{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./bat.nix
    ./exa.nix
    ./htop.nix
    ./prettyping.nix
    ./strace.nix
    ./tcpdump.nix
  ];
}

{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./atcoder-tools.nix
    ./bat.nix
    ./exa.nix
    ./htop.nix
    ./prettyping.nix
    ./ripgrep.nix
    ./strace.nix
    ./tcpdump.nix
  ];
}

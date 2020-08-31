{ lib, ... }:

with lib; {
  imports = [
    ./atcoder-tools.nix
    ./bat.nix
    ./exa.nix
    ./htop.nix
    ./prettyping.nix
    ./ranger.nix
    ./ripgrep.nix
    ./strace.nix
    ./tcpdump.nix
  ];
}

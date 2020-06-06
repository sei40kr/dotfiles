{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./editors
    ./git.nix
    ./cc.nix
    ./go.nix
    ./groovy.nix
    ./haskell.nix
    ./java.nix
    ./kotlin.nix
    ./python.nix
    ./r.nix
    ./ruby.nix
    ./rust.nix
  ];
}

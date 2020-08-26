{ lib, ... }:

with lib; {
  imports = [
    ./editors
    ./tools
    ./cc.nix
    ./go.nix
    ./groovy.nix
    ./haskell.nix
    ./java.nix
    ./javascript.nix
    ./kotlin.nix
    ./python.nix
    ./r.nix
    ./ruby.nix
    ./rust.nix
    ./scala.nix
    ./sh.nix
    ./web.nix
  ];
}

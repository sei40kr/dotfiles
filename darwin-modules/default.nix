{ inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.darwinModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  user.packages = with pkgs; [
    coreutils
    diffutils
    findutils
    gnugrep
    gnumake
    gnutar
    gnused
    gzip
    libtool
  ];
}

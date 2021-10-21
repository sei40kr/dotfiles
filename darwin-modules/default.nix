{ lib, pkgs, ... }:

with lib;
with lib.my; {
  config = {
    imports = [ home-manager.darwinModules.home-manager ]
      ++ (mapModulesRec' (toString ./.) import);

    nix.useDaemon = true;

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
  };
}

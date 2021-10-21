{ lib, pkgs, ... }:

with lib;
with lib.my; {
  config = {
    imports = [ home-manager.darwinModules.home-manager ]
      ++ (mapModulesRec' (toString ../modules) import)
      ++ (mapModulesRec' (toString ./.) import);

    nix = {
      extraOptions = "experimental-features = nix-command flakes";
      package = pkgs.nixFlakes;
      useDaemon = true;
    };

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

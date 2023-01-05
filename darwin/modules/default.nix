{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [ inputs.home-manager.darwinModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  config = {
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

    nix.useDaemon = true;

    fonts.fontDir.enable = true;

    system.build.applications = mkForce (pkgs.buildEnv {
      name = "system-applications";
      paths = config.environment.systemPackages
        ++ config.users.users.${config.user.name}.packages;
      pathsToLink = "/Applications";
    });
  };
}

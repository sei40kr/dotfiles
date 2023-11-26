{ config, lib, options, pkgs, ... }:

with lib;
with lib.my;
{
  imports = mapModulesRec' (toString ./.) import;

  options = with types; {
    fonts.packages = mkOption {
      type = listOf path;
      default = [ ];
      description = mkDoc ''
        List of primary font packages.
      '';
    };
  };

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

    fonts = {
      fontDir.enable = true;
      # Workaround for LnL7/nix-darwin#752
      fonts = mkAliasDefinitions options.fonts.packages;
    };

    system.build.applications = mkForce (pkgs.buildEnv {
      name = "system-applications";
      paths = config.environment.systemPackages
        ++ config.users.users.${config.user.name}.packages;
      pathsToLink = "/Applications";
    });
  };
}

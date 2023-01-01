{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
{
  imports = [ inputs.home-manager.darwinModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  nix.useDaemon = true;

  system.activationScripts.applications.text =
    let
      env = pkgs.buildEnv {
        name = "applications";
        paths = config.environment.systemPackages
          ++ config.users.users.${config.user.name}.packages;
        pathsToLink = "/Applications";
      };
    in
    mkForce ''
      # Set up applications.
      echo "setting up ~/Applications/Nix Apps..." >&2

      rm -rf ~/Applications/Nix\ Apps
      mkdir -p ~/Applications/Nix\ Apps

      srcs=()
      while read src; do
        srcs+=("$src")
      done < <(find ${env}/Applications -maxdepth 1 -type l)
      if [[ "''${#srcs[@]}" != 0 ]]; then
        /bin/cp -LR "''${srcs[@]}" ~/Applications/Nix\ Apps
      fi
    '';

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

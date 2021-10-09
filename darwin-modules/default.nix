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

    users.nix.configureBuildUsers = true;

    system.build.applications = pkgs.buildEnv {
      name = "user-applications";
      paths = config.users.users.${config.user.name}.packages;
      pathsToLink = "/Applications";
    };

    user.home = "/Users/${config.user.name}";

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

    modules.shell.zsh.rcInit = ''
      zinit ice id-as'PZT::modules--gnu-utility'
      zinit light ${pkgs.zsh-prezto}/modules/gnu-utility
    '';
  };
}

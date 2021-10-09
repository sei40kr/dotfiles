{ config, lib, options, pkgs, ... }:

with lib;
with lib.my;
let inherit (pkgs.stdenv) isDarwin;
in {
  options = with types; {
    user = mkOpt attrs { };

    dotfiles = {
      # TODO
      dir = mkOpt (either str path) "${../.}";
      binDir = mkOpt (either str path) "${config.dotfiles.dir}/bin";
      configDir = mkOpt (either str path) "${config.dotfiles.dir}/config";
    };

    home = {
      file = mkOpt' attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (_: v:
        if isList v then concatMapStringsSep ":" toString v else (toString v));
      default = { };
    };
  };

  config = {
    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          # Necessary for home-manager to work with flakes, otherwise it will
          # look for a nixpkgs channel.
          stateVersion = if isDarwin then
            config.system.nixpkgsRelease
          else
            config.system.stateVersion;
        };
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix = let users = [ "root" config.user.name ];
    in {
      trustedUsers = users;
      allowedUsers = users;
    };

    env.PATH = [ "\${PATH}" ];
  };
}

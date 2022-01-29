{ config, inputs, lib, options, pkgs, ... }:

with lib;
with lib.my;
let inherit (pkgs.stdenv) isDarwin;
in {
  options = with types; {
    user = mkOpt attrs { };

    dotfiles = let t = either str path;
    in {
      dir = mkOpt t (findFirst pathExists (toString ../.) [
        "${config.user.home}/.config/dotfiles"
        "/etc/dotfiles"
      ]);
      binDir = mkOpt t "${config.dotfiles.dir}/bin";
      configDir = mkOpt t "${config.dotfiles.dir}/config";
      secretsDir = mkOpt t inputs.secrets;
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
    user = {
      description = "The primary user account";
      uid = 1000;
    };

    home-manager = {
      useUserPackages = true;

      users.${config.user.name} = {
        home.file = mkAliasDefinitions options.home.file;
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    users.users.${config.user.name} = mkAliasDefinitions options.user;

    nix.settings = let users = [ "root" config.user.name ];
    in {
      trusted-users = users;
      allowed-users = users;
    };

    env.PATH = [ "$DOTFILES_BIN" "$XDG_BIN_HOME" "$PATH" ];
  };
}

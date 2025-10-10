{
  config,
  lib,
  options,
  ...
}:

let
  inherit (lib)
    concatMapStringsSep
    findFirst
    mapAttrs
    mkAliasDefinitions
    mkOption
    pathExists
    types
    ;
  inherit (types)
    attrs
    either
    isList
    oneOf
    path
    str
    ;
  inherit (lib.my) mkOpt mkOpt';
in
{
  options = {
    user = mkOpt attrs { };

    dotfiles =
      let
        t = either str path;
      in
      {
        dir = mkOpt t (
          findFirst pathExists (toString ../.) [
            "${config.user.home}/.config/dotfiles"
            "/etc/dotfiles"
          ]
        );
        binDir = mkOpt t "${config.dotfiles.dir}/bin";
        configDir = mkOpt t "${config.dotfiles.dir}/config";
      };

    home = {
      file = mkOpt' types.attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' types.attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' types.attrs { } "Files to place in $XDG_DATA_HOME";
    };

    env = mkOption {
      type = types.attrsOf (oneOf [
        types.str
        path
        (types.listOf (types.either types.str path))
      ]);
      apply = mapAttrs (_: v: if isList v then concatMapStringsSep ":" toString v else (toString v));
      default = { };
    };
  };

  config = {
    user = {
      description = "The primary user account";
      uid = 1000;
      isNormalUser = true;
      group = "users";
      extraGroups = [ "wheel" ];
      home = "/home/${config.user.name}";
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

    env.PATH = [
      "$DOTFILES_BIN"
      "$XDG_BIN_HOME"
      "$PATH"
    ];
  };
}

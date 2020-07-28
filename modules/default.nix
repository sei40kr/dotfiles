{ config, lib, options, ... }:

with lib; {
  imports = [ ./desktop ./dev ./services ./shell ];

  options.my = {
    userName = mkOption { type = types.str; };
    userEmail = mkOption { type = types.str; };

    user = mkOption { type = types.submodule; };
    packages = mkOption {
      type = with types; listOf package;
      default = [ ];
    };

    env = mkOption {
      type = with types;
        attrsOf (either (either str path) (listOf (either str path)));
      apply =
        mapAttrs (_: v: if isList v then concatStringsSep ":" v else "${v}");
    };

    aliases = mkOption {
      type = with types; attrsOf str;
      default = { };
    };

    xsession = {
      init = mkOption {
        type = types.lines;
        default = "";
      };
    };
  };

  config = {
    users.users.${config.my.userName} = mkAliasDefinitions options.my.user;

    my.env = {
      GIO_EXTRA_MODULES = [ "\${GIO_EXTRA_MODULES}" ];
      PATH = [ "\${PATH}" ];
      XDG_DATA_DIRS = [ "\${XDG_DATA_DIRS}" ];
    };

    my.home = {
      home.packages = config.my.packages;
      home.sessionVariables = config.my.env;
    };

    modules.shell.zsh.aliases = config.my.aliases;
  };
}

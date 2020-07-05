{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    <home-manager/nixos>

    ./desktop
    ./dev
    ./media
    ./services
    ./shell
    ./themes
  ];

  options.my = {
    userName = mkOption { type = types.str; };
    userEmail = mkOption { type = types.str; };

    home = mkOption { type = options.home-manager.users.type.functor.wrapped; };
    user = mkOption { type = types.submodule; };
    packages = mkOption { type = with types; listOf package; };

    env = mkOption {
      type = with types;
        attrsOf (either (either str path) (listOf (either str path)));
      apply = mapAttrs (n: v:
        if isList v then
          concatMapStringsSep ":" (x: toString x) v
        else
          (toString v));
    };

    xsession = {
      init = mkOption {
        type = types.lines;
        default = "";
      };
    };

    zsh = {
      aliases = mkOption {
        type = with types; attrsOf str;
        default = { };
      };
    };
  };

  config = {
    home-manager.users.${config.my.userName} =
      mkAliasDefinitions options.my.home;
    users.users.${config.my.userName} = mkAliasDefinitions options.my.user;

    my.env = {
      GIO_EXTRA_MODULES = [ "\${GIO_EXTRA_MODULES}" ];
      PATH = [ "\${PATH}" ];
      XDG_DATA_DIRS = [ "\${XDG_DATA_DIRS}" ];
    };

    my.home = {
      home = {
        packages = config.my.packages;
        sessionVariables = config.my.env;
      };

      programs.zsh.shellAliases = config.my.zsh.aliases;
    };
  };
}

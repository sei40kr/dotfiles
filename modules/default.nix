{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    (import "${
        builtins.fetchTarball
        "https://github.com/rycee/home-manager/archive/master.tar.gz"
      }/nixos")

    ./desktop
    ./dev
    ./shell
    ./system
  ];

  options.my = {
    userName = mkOption { type = types.str; };

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

    my.env.PATH = [ "$PATH" ];

    my.home = {
      home = {
        packages = config.my.packages;
        sessionVariables = config.my.env;
      };

      xsession = {
        initExtra = config.my.xsession.init;
        importedVariables = [ "PATH" ];
      };

      programs.zsh.shellAliases = config.my.zsh.aliases;
    };
  };
}

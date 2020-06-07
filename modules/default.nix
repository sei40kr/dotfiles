{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [ ./desktop ./dev ./shell ];

  options.my = {
    env = mkOption { type = types.attrs; };
    packages = mkOption { type = with types; listOf package; };

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
    my.env = { PATH = [ "$PATH" ]; };

    home = {
      packages = config.my.packages;
      sessionVariables = config.my.env;
    };
    xsession.initExtra = config.my.xsession.init;
    programs.zsh = { shellAliases = config.my.zsh.aliases; };
  };
}

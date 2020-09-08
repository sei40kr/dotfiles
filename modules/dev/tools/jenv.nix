{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.dev.tools.jenv;
  package = pkgs.my.jenv.override {
    inherit (cfg) javaPackages;

    plugins = cfg.pluginsToEnable;
  };
in {
  options.modules.dev.tools.jenv = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    javaPackages = mkOption {
      type = types.attrsOf types.package;
      default = { };
    };

    # TODO Rename pluginsToEnable -> plugins
    pluginsToEnable = mkOption {
      type = with types; listOf str;
      default = [ "export" ];
    };
  };

  config = mkIf cfg.enable {
    modules.shell.zsh.zinitPluginsInit = ''
      zinit ice atclone'jenv init - --no-rehash zsh >jenv-init.zsh' \
                atpull'%atclone' \
                id-as'jenv-init'
      zinit light zdharma/null
    '';

    my.packages = [ package ];
    my.env = rec {
      JENV_ROOT = "\${HOME}/.jenv";
      PATH = [ "${JENV_ROOT}/bin" "${JENV_ROOT}/shims" ];
    };
    my.home.home.file.".jenv".source = "${package}/share/jenv";
  };
}

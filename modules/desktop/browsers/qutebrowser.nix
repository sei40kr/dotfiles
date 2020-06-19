{ config, lib, options, pkgs, ... }:

with lib;
let cfg = config.modules.desktop.browsers.qutebrowser;
in {
  options.modules.desktop.browsers.qutebrowser = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };

    themeConfig = mkOption {
      type = types.path;
      default = null;
    };
  };

  config = mkIf cfg.enable {
    my.home.programs.qutebrowser = {
      enable = true;
      extraConfig = ''
        config.source('${<config/qutebrowser/config.py>}')

        ${optionalString (cfg.themeConfig != null)
        "config.source('${cfg.themeConfig}')"}
      '';
    };
  };
}

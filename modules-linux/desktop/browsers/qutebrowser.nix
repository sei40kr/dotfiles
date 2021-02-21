{ config, home-manager, lib, ... }:

with lib;
with lib.my;
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
    home-manager.users.${config.user.name}.programs.qutebrowser = {
      enable = true;
      extraConfig = ''
        config.source('${configDir}/qutebrowser/config.py')

        ${optionalString (cfg.themeConfig != null)
        "config.source('${cfg.themeConfig}')"}
      '';
    };
  };
}

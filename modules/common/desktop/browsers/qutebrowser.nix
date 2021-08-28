{ config, home-manager, lib, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.browsers.qutebrowser;
in {
  options.modules.desktop.browsers.qutebrowser = {
    enable = mkOption {
      type = types.bool;
      default = false;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.qutebrowser = {
      enable = true;
      extraConfig = ''
        config.source('${configDir}/qutebrowser/config.py')
      '';
    };
  };
}

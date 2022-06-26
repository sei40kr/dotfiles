{ config, lib, ... }:

with builtins;
with lib;
with lib.my;
let
  browsersCfg = config.modules.desktop.browsers;
  cfg = browsersCfg.psd;

  browsers = attrNames (filterAttrs (_n: v: v) {
    firefox = browsersCfg.firefox.enable;
    google-chrome = browsersCfg.chrome.enable;
  });
in
{
  options.modules.desktop.browsers.psd = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.psd.enable = true;

    home.configFile."psd/psd.conf".text = ''
      #
      # $XDG_CONFIG_HOME/psd/psd.conf
      #
      # For documentation, refer man 1 psd or to the wiki page
      # https://wiki.archlinux.org/index.php/Profile-sync-daemon

      BROWSERS=( ${escapeShellArgs browsers} )
    '';
  };
}

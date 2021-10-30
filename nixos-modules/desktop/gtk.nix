{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gtk;
  themeCfg = cfg.theme;
in {
  options.modules.desktop.gtk = with types; {
    enable = mkBoolOpt false;
    font = mkOpt (nullOr attrs) null;
    theme = {
      iconTheme = mkOpt (nullOr attrs) null;
      theme = mkOpt (nullOr attrs) null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.gtk = {
      inherit (cfg) enable font;
      inherit (themeCfg) iconTheme theme;
      gtk2.extraConfig = ''
        gtk-key-theme-name = "Emacs"
      '';
      gtk3.extraConfig = {
        gtk-enable-primary-paste = false;
        gtk-key-theme-name = "Emacs";
      };
    };
  };
}

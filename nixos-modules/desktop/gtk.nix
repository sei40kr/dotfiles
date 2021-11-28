{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gtk;

  keyTheme = "Emacs";
in {
  options.modules.desktop.gtk = with types; {
    enable = mkBoolOpt false;

    font = mkOpt (nullOr attrs) null;

    iconTheme = mkOpt (nullOr attrs) null;

    theme = mkOpt (nullOr attrs) null;
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name} = {
      gtk = {
        inherit (cfg) font iconTheme theme;

        enable = true;

        gtk2.extraConfig = ''
          gtk-key-theme-name = "${keyTheme}"
        '';

        gtk3.extraConfig = {
          gtk-enable-primary-paste = false;
          gtk-key-theme-name = keyTheme;
        };
      };
    };

    modules.desktop.dconf = {
      enable = true;

      settings = {
        "org/gnome/desktop/interface" = {
          icon-theme = mkIf (cfg.iconTheme != null) cfg.iconTheme.name;
          gtk-theme = mkIf (cfg.theme != null) cfg.theme.name;
          gtk-key-theme = keyTheme;
        };
      };
    };
  };
}

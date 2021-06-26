{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.gtk;
  themeType = with types;
    submodule {
      options = {
        package = mkOpt package null;
        name = mkOpt str null;
      };
    };
  colorsCfg = config.modules.theme.colors;
in {
  options.modules.desktop.gtk = with types; {
    enable = mkBoolOpt false;
    theme = {
      iconTheme = mkOpt (nullOr themeType) null;
      theme = mkOpt (nullOr themeType) null;
    };
  };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.gtk = {
      inherit (cfg.theme) iconTheme theme;
      enable = true;
      gtk3 = {
        extraConfig = {
          gtk-application-prefer-dark-theme =
            (config.modules.theme.variant == "dark");
          gtk-enable-primary-paste = false;
        };
        extraCss = ''
          *:selected,
          selection,
          menu menuitem:hover,
          switch:checked {
            background-color: ${colorsCfg.selection.bg};
            color: ${colorsCfg.selection.text};
          }
        '';
      };
    };
  };
}

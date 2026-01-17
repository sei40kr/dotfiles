{
  config,
  inputs,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib) mkIf mkOption types;
  inherit (types) enum;

  themeCfg = config.modules.desktop.theme;
  cfg = themeCfg.whitesur;
in
{
  options.modules.desktop.theme.whitesur = {
    variant = mkOption {
      type = enum [
        "light"
        "dark"
      ];
      default = "light";
      description = "Color variant for WhiteSur theme";
    };
  };

  config = mkIf (themeCfg.active == "whitesur") {
    modules.desktop.de.background.image = {
      path = "${perSystem.self.whitesur-wallpapers}/share/backgrounds/WhiteSur-${cfg.variant}.png";
      mode = "fill";
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 12;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 12;

      normal = {
        background = if cfg.variant == "light" then "#f5f5f5" else "#333333";
        foreground = if cfg.variant == "light" then "#363636" else "#dadada";
      };
    };
  };
}

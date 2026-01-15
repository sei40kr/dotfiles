{
  config,
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
  };
}

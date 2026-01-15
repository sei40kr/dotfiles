{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkOption types;
  inherit (types) enum;

  themeCfg = config.modules.desktop.theme;
  cfg = themeCfg.graphite;
  inherit (cfg.variant) color;

  Color =
    if cfg.variant.color == "light" then
      "Light"
    else if cfg.variant.color == "dark" then
      "Dark"
    else
      abort "Invalid color variant (variant.color = ${cfg.variant.color})";

  gtkTheme = pkgs.graphite-gtk-theme.override { wallpapers = true; };
in
{
  options.modules.desktop.theme.graphite = {
    variant = {
      color = mkOption {
        type = enum [
          "light"
          "dark"
        ];
        default = "light";
        description = "Color variant for Graphite theme";
      };
    };
  };

  config = mkIf (themeCfg.active == "graphite") {
    modules.desktop.de.background.image = {
      path = "${gtkTheme}/share/backgrounds/wave-${Color}.jpg";
      mode = "fill";
    };
  };
}

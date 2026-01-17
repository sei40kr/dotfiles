{
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf optionalString;

  themeCfg = osConfig.modules.desktop.theme;
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
  config = mkIf (themeCfg.active == "graphite") {
    gtk = {
      enable = true;
      theme = {
        package = gtkTheme;
        name = "Graphite-${Color}";
      };
      iconTheme = {
        package = pkgs.tela-circle-icon-theme.override { colorVariants = [ "black" ]; };
        name = "Tela-circle-black${optionalString (color == "dark") "-dark"}";
      };
    };
  };
}

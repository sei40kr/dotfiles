{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) attrByPath mkIf optionalString;

  themeCfg = attrByPath [ "modules" "desktop" "theme" ] { active = null; } osConfig;
  cfg = themeCfg.graphite or { };
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
      gtk4.theme = config.gtk.theme;
      iconTheme = {
        package = pkgs.tela-circle-icon-theme.override { colorVariants = [ "black" ]; };
        name = "Tela-circle-black${optionalString (color == "dark") "-dark"}";
      };
    };
  };
}

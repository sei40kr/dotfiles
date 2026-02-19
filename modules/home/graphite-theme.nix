{
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
      iconTheme = {
        package = pkgs.tela-circle-icon-theme.override { colorVariants = [ "black" ]; };
        name = "Tela-circle-black${optionalString (color == "dark") "-dark"}";
      };
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 12;
        y = 6;
        textIcon = 6;
      };

      borderWidth = 2;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 12;

      normal = {
        background = if color == "light" then "#f9fafb" else "#2c2c2c";
        foreground = if color == "light" then "#000000de" else "#ffffff";
        borderColor = if color == "light" then "#000000de" else "#ffffff";
      };
    };
  };
}

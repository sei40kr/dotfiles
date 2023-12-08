{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  themeCfg = config.modules.desktop.theme;
  cfg = themeCfg.graphite;

  inherit (cfg.variant) color;
  Color =
    if cfg.variant.color == "light" then "Light"
    else if cfg.variant.color == "dark" then "Dark"
    else abort "Invalid color variant (variant.color = ${cfg.variant.color})";

  gtkTheme = pkgs.graphite-gtk-theme.override { wallpapers = true; };
in
{
  options.modules.desktop.theme.graphite = with types; {
    variant = {
      color = mkOpt (types.enum [ "light" "dark" ]) "light";
    };
  };

  config = mkIf (themeCfg.active == "graphite") {
    modules.desktop.de.background.image = {
      path = "${gtkTheme}/share/backgrounds/wave-${Color}.png";
      mode = "fit";
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

    # TODO: support hi-dpi theme variants
    modules.desktop.gtk.theme = {
      package = gtkTheme;
      name = "Graphite-${Color}";
    };

    modules.desktop.gtk.iconTheme = {
      package = pkgs.tela-circle-icon-theme.override { colorVariants = [ "black" ]; };
      name = "Tela-circle-black${optionalString (color == "dark") "-dark"}";
    };

    modules.desktop.qt.kvantum.theme = {
      package = pkgs.graphite-kde-theme;
      dir = "Graphite";
      name = "Graphite${optionalString (color == "dark") "Dark"}";
    };
  };
}


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
    modules.desktop.background.image = {
      path = "${gtkTheme}/share/backgrounds/wave-${color}.png";
      mode = "fit";
    };

    # TODO: install Graphite cursor theme

    modules.desktop.gnome.shell.theme = {
      package = gtkTheme;
      name = "Graphite-${Color}";
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


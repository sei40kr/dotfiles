{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf optionalString types;
  inherit (types) enum;
  inherit (lib.my) mkOpt;

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
      color = mkOpt (enum [
        "light"
        "dark"
      ]) "light";
    };
  };

  config = mkIf (themeCfg.active == "graphite") {
    modules.desktop.de.background.image = {
      path = "${gtkTheme}/share/backgrounds/wave-${Color}.png";
      mode = "fill";
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

    modules.desktop.apps.rofi = {
      window = {
        padding = {
          x = 12;
          y = 12;
        };

        border = {
          width = 2;
          color = if color == "light" then "#000000de" else "#ffffff";
        };

        cornerRadius = 12;

        bg = if color == "light" then "#f9fafb" else "#2c2c2c";
        fg = if color == "light" then "#000000de" else "#ffffff";
      };

      input = {
        margin.y = 12;

        padding = {
          x = 12;
          y = 9;
        };

        cornerRadius = 6;

        bg = if color == "light" then "#e0e0e0" else "#333333";
        fg = if color == "light" then "#000000de" else "#ffffff";

        prompt = {
          margin.x = 12;
          fg = if color == "light" then "#00000099" else "#ffffffb3";
        };

        info = {
          margin.x = 12;
          fg = if color == "light" then "#00000099" else "#ffffffb3";
        };
      };

      item = {
        iconSize = 24;

        padding = {
          x = 12;
          y = 9;
          textIcon = 12;
        };

        cornerRadius = 6;

        normal = {
          bg = if color == "light" then "#f9fafb" else "#2c2c2c";
          fg = if color == "light" then "#000000de" else "#ffffff";
          highlight = {
            fg = if color == "light" then "#000000de" else "#ffffff";
            fontStyles = [ "bold" ];
          };
        };

        selected = {
          bg = if color == "light" then "#000000de" else "#ffffff";
          fg = if color == "light" then "#f9fafb" else "#2c2c2c";
          highlight = {
            fg = if color == "light" then "#ffffff" else "#000000de";
            fontStyles = [ "bold" ];
          };
        };
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

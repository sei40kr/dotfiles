{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;

  themeCfg = config.modules.desktop.theme;

  orchis-theme = pkgs.orchis-theme.override { withWallpapers = true; };
in
{
  config = mkIf (themeCfg.active == "orchis") {
    modules.desktop.fontconfig.fonts.sansSerif = {
      packages = with pkgs; [
        roboto
        noto-fonts
        noto-fonts-cjk
      ];
      names = [
        "Roboto"
        "Noto Sans Mono"
        "Noto Sans Mono CJK JP"
      ];
    };

    modules.desktop.de.background.image = {
      path = "${orchis-theme}/share/backgrounds/4k.jpg";
      mode = "fill";
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 18;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 18;

      normal = {
        background = "#f5f5f5";
        foreground = "#000000de";
      };
    };

    modules.desktop.apps.rofi = {
      window = {
        cornerRadius = 18;

        padding = {
          x = 18;
          y = 18;
        };

        bg = "#f5f5f5";
        fg = "#000000de";
      };

      input = {
        margin.y = 12;

        padding = {
          x = 18;
          y = 8;
        };

        border = {
          width = 2;
          color = "#1a73e8";
        };

        cornerRadius = 9999;

        bg = "#ffffff";
        fg = "#000000de";

        prompt = {
          margin.x = 12;
          fg = "#00000099";
        };

        info = {
          margin.x = 12;
          fg = "#00000099";
        };
      };

      item = {
        iconSize = 24;

        padding = {
          x = 18;
          y = 8;
          textIcon = 12;
        };

        cornerRadius = 9999;

        normal.highlight = {
          fg = "#1a73e8";
          fontStyles = [ "bold" ];
        };

        selected = {
          bg = "#1a73e8";
          fg = "#ffffff";
          highlight = {
            fg = "#ffffff";
            fontStyles = [ "bold" ];
          };
        };
      };
    };

    modules.desktop.gtk = {
      font = {
        package = pkgs.roboto;
        name = "sans-serif";
        size = 11;
      };
      iconTheme = {
        package = pkgs.tela-icon-theme;
        name = "Tela";
      };
      theme = {
        package = orchis-theme;
        name = "Orchis";
      };
    };
  };
}

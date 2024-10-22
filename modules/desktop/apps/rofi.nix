{ config, lib, pkgs, ... }:

let
  inherit (lib) concatStringsSep max mdDoc mkEnableOption mkIf mkOption types;
  inherit (types) enum int listOf str;
  inherit (lib.my.extraTypes) font;
  inherit (config.dotfiles) configDir;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.apps.rofi;
in
{
  options.modules.desktop.apps.rofi = {
    enable = mkEnableOption (mdDoc "Rofi");

    window = {
      width = mkOption {
        type = int;
        default = 640;
        description = "The width in pixels of the Rofi window.";
      };

      padding = {
        x = mkOption {
          type = int;
          default = 0;
          description = "The horizontal padding in pixels of the Rofi window.";
        };

        y = mkOption {
          type = int;
          default = 0;
          description = "The vertical padding in pixels of the Rofi window.";
        };
      };

      border = {
        width = mkOption {
          type = int;
          default = 0;
          description = "The width in pixels of the Rofi window border.";
        };

        color = mkOption {
          type = str;
          default = "#00000000";
          description = "The color of the Rofi window border.";
        };
      };

      cornerRadius = mkOption {
        type = int;
        default = 0;
        description = "The corner radius in pixels of the Rofi window.";
      };

      bg = mkOption {
        type = str;
        example = "#ffffff";
        description = "The background color of the Rofi window.";
      };

      fg = mkOption {
        type = str;
        example = "#000000";
        description = "The foreground color of the Rofi window.";
      };
    };

    input = {
      margin = {
        y = mkOption {
          type = int;
          default = 0;
          description = "The margin in pixels between the Rofi input box and the Rofi items.";
        };
      };

      padding = {
        x = mkOption {
          type = int;
          default = 0;
          description = "The horizontal padding in pixels of the Rofi input box.";
        };

        y = mkOption {
          type = int;
          default = 0;
          description = "The vertical padding in pixels of the Rofi input box.";
        };
      };

      border = {
        width = mkOption {
          type = int;
          default = 0;
          description = "The width in pixels of the Rofi input box border.";
        };

        color = mkOption {
          type = str;
          default = "#00000000";
          description = "The color of the Rofi input box border.";
        };
      };

      cornerRadius = mkOption {
        type = int;
        default = 0;
        description = "The corner radius in pixels of the Rofi input box.";
      };

      bg = mkOption {
        type = str;
        default = "#00000000";
        description = "The background color of the Rofi input box.";
      };

      fg = mkOption {
        type = str;
        default = cfg.window.fg;
        description = "The foreground color of the Rofi input box.";
      };

      font = mkOption {
        type = font;
        default = deCfg.defaultFonts.ui;
        description = "The font of the Rofi input box.";
      };

      prompt = {
        margin = {
          x = mkOption {
            type = int;
            default = 0;
            description = "The horizontal margin in pixels of the Rofi input box prompt.";
          };
        };

        fg = mkOption {
          type = str;
          default = cfg.input.fg;
          description = "The foreground color of the Rofi input box prompt.";
        };

        font = mkOption {
          type = font;
          default = cfg.input.font;
          description = "The font of the Rofi input box prompt.";
        };
      };

      info = {
        margin = {
          x = mkOption {
            type = int;
            default = 0;
            description = "The horizontal margin in pixels of the Rofi input box info.";
          };
        };

        fg = mkOption {
          type = str;
          default = cfg.input.fg;
          description = "The foreground color of the Rofi input box info.";
        };

        font = mkOption {
          type = font;
          default = cfg.input.font;
          description = "The font of the Rofi input box info.";
        };
      };
    };

    list = {
      spacing = mkOption {
        type = int;
        default = 0;
        description = "The spacing in pixels between the Rofi items.";
      };
    };

    item = {
      iconSize = mkOption {
        type = int;
        default = 0;
        description = "The size in pixels of the icons in the Rofi window.";
      };

      padding = {
        x = mkOption {
          type = int;
          default = 0;
          description = "The horizontal padding in pixels of the Rofi items.";
        };

        y = mkOption {
          type = int;
          default = 0;
          description = "The vertical padding in pixels of the Rofi items.";
        };

        textIcon = mkOption {
          type = int;
          default = 0;
          description = "The horizontal padding in pixels between the text and icon of the Rofi items.";
        };
      };

      cornerRadius = mkOption {
        type = int;
        default = 0;
        description = "The corner radius in pixels of the Rofi items.";
      };

      font = mkOption {
        type = font;
        default = deCfg.defaultFonts.ui;
        description = "The font of the Rofi items.";
      };

      normal = {
        bg = mkOption {
          type = str;
          default = "#00000000";
          description = "The background color of the Rofi items.";
        };

        fg = mkOption {
          type = str;
          default = cfg.window.fg;
          description = "The foreground color of the Rofi items.";
        };

        highlight = {
          fg = mkOption {
            type = str;
            default = cfg.item.normal.fg;
            description = "The search highlight color of the Rofi items.";
          };

          fontStyles = mkOption {
            type = listOf (enum [ "bold" "underline" "italic" ]);
            default = [ ];
            description = "The font styles of the search highlight of the Rofi items.";
          };
        };
      };

      selected = {
        bg = mkOption {
          type = str;
          default = cfg.item.normal.bg;
          description = "The background color of the selected Rofi item.";
        };

        fg = mkOption {
          type = str;
          default = cfg.item.normal.fg;
          description = "The foreground color of the selected Rofi item.";
        };

        highlight = {
          fg = mkOption {
            type = str;
            default = cfg.item.normal.highlight.fg;
            description = "The search highlight color of the selected Rofi item.";
          };

          fontStyles = mkOption {
            type = listOf (enum [ "bold" "underline" "italic" ]);
            default = [ ];
            description = "The font styles of the search highlight of the Rofi items.";
          };
        };
      };
    };
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ rofi-wayland ];

    home.configFile."rofi/config.rasi".source = "${configDir}/rofi/rofi.rasi";
    home.configFile."rofi/themes/mytheme.rasi".text = ''
      window {
        width: ${toString cfg.window.width}px;
        padding: ${toString cfg.window.padding.y}px ${toString cfg.window.padding.x}px;
        border: ${toString cfg.window.border.width}px solid;
        border-color: ${cfg.window.border.color};
        border-radius: ${toString cfg.window.cornerRadius}px;
        background-color: ${cfg.window.bg};
        text-color: ${cfg.window.fg};
      }

      mainbox {
        background-color: #00000000;
      }

      inputbar {
        margin: ${toString (max (cfg.input.margin.y - cfg.window.padding.y) 0)} 0 ${toString cfg.input.margin.y}px 0;
        padding: ${toString cfg.input.padding.y}px ${toString cfg.input.padding.x}px;
        border: ${toString cfg.input.border.width}px solid;
        border-color: ${cfg.input.border.color};
        border-radius: ${toString cfg.input.cornerRadius}px;
        background-color: ${cfg.input.bg};
        children: [prompt, entry, num-filtered-rows, textbox-slash, num-rows];
      }

      prompt {
        margin: 0 ${toString cfg.input.prompt.margin.x} 0 ${toString (max (cfg.input.prompt.margin.x - cfg.input.padding.x) 0)}px;
        background-color: #00000000;
        text-color: ${cfg.input.prompt.fg};
        font: "${cfg.input.prompt.font.name} ${toString cfg.input.prompt.font.size}";
      }

      entry {
        background-color: #00000000;
        text-color: ${cfg.input.fg};
        font: "${cfg.input.font.name} ${toString cfg.input.font.size}";
      }

      num-filtered-rows,
      textbox-slash,
      num-rows {
        background-color: #00000000;
        text-color: ${cfg.input.info.fg};
        font: "${cfg.input.info.font.name} ${toString cfg.input.info.font.size}";
      }

      num-filtered-rows {
        margin: 0 0 0 ${toString cfg.input.info.margin.x}px;
      }

      textbox-slash {
        expand: false;
        content: "/";
      }

      num-rows {
        margin: 0 ${toString (max (cfg.input.info.margin.x - cfg.input.padding.x) 0)}px 0 0;
      }

      listview {
        fixed-height: true;
        spacing: ${toString cfg.list.spacing}px;
        background-color: #00000000;
        lines: 10;
      }

      element {
        padding: ${toString cfg.item.padding.y}px ${toString cfg.item.padding.x}px;
        border-radius: ${toString cfg.item.cornerRadius}px;
        background-color: ${cfg.item.normal.bg};
        children: [element-icon, element-text];
      }
      element selected {
        background-color: ${cfg.item.selected.bg};
      }

      element-icon {
        size: ${toString cfg.item.iconSize};
        margin: 0 ${toString cfg.item.padding.textIcon}px 0 0;
        background-color: #00000000;
      }

      element-text {
        background-color: #00000000;
        text-color: ${cfg.item.normal.fg};
        highlight: ${concatStringsSep " " cfg.item.normal.highlight.fontStyles} ${cfg.item.normal.highlight.fg};
        font: "${cfg.item.font.name} ${toString cfg.item.font.size}";
        vertical-align: 0.5;
        markup: true;
      }
      element-text selected {
        text-color: ${cfg.item.selected.fg};
        highlight: ${concatStringsSep " " cfg.item.selected.highlight.fontStyles} ${cfg.item.selected.highlight.fg};
      }
    '';

    modules.term.sensible.enable = true;
  };
}

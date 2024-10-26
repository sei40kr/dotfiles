{ lib, ... }:

with lib;
with lib.my;
let
  statusLineSectionType =
    with types;
    submodule {
      options = {
        fg = mkOption {
          type = str;
          default = null;
          description = mdDoc ''
            The color of the status line section foreground.
          '';
          visible = false;
        };

        bg = mkOption {
          type = str;
          default = null;
          description = mdDoc ''
            The color of the status line section background.
          '';
          visible = false;
        };
      };
    };
in
{
  options.modules.term.colorschemes = with types; {
    active = mkOption {
      type = enum [ "tokyo-night" ];
      default = null;
      description = lib.mdDoc ''
        The name of the active colorscheme.
      '';
    };

    colors = {
      fg = mkOption {
        type = str;
        default = null;
        description = lib.mdDoc ''
          The color of the foreground.
        '';
        visible = false;
      };

      bg = mkOption {
        type = str;
        default = null;
        description = lib.mdDoc ''
          The color of the background.
        '';
        visible = false;
      };

      ansi = {
        black = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the black ansi color.
          '';
          visible = false;
        };

        red = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the red ansi color.
          '';
          visible = false;
        };

        green = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the green ansi color.
          '';
          visible = false;
        };

        yellow = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the yellow ansi color.
          '';
          visible = false;
        };

        blue = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the blue ansi color.
          '';
          visible = false;
        };

        magenta = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the magenta ansi color.
          '';
          visible = false;
        };

        cyan = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the cyan ansi color.
          '';
          visible = false;
        };

        white = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the white ansi color.
          '';
          visible = false;
        };

        brightBlack = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright black ansi color.
          '';
          visible = false;
        };

        brightRed = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright red ansi color.
          '';
          visible = false;
        };

        brightGreen = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright green ansi color.
          '';
          visible = false;
        };

        brightYellow = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright yellow ansi color.
          '';
          visible = false;
        };

        brightBlue = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright blue ansi color.
          '';
          visible = false;
        };

        brightMagenta = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright magenta ansi color.
          '';
          visible = false;
        };

        brightCyan = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright cyan ansi color.
          '';
          visible = false;
        };

        brightWhite = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the bright white ansi color.
          '';
          visible = false;
        };
      };

      cursor = {
        fg = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the cursor.
          '';
          visible = false;
        };

        bg = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the cursor background.
          '';
          visible = false;
        };
      };

      link = mkOption {
        type = str;
        default = null;
        description = lib.mdDoc ''
          The color of the links.
        '';
        visible = false;
      };

      selection = {
        fg = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the selection foreground.
          '';
          visible = false;
        };

        bg = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the selection background.
          '';
          visible = false;
        };
      };

      paneBorder = {
        focused = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the active pane border.
          '';
          visible = false;
        };

        default = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the default pane border.
          '';
          visible = false;
        };

        urgent = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the urgent pane border.
          '';
          visible = false;
        };
      };

      tabBar = {
        bg = mkOption {
          type = str;
          default = null;
          description = lib.mdDoc ''
            The color of the tab bar background.
          '';
          visible = false;
        };

        activeTab = {
          bg = mkOption {
            type = str;
            default = null;
            description = lib.mdDoc ''
              The color of the active tab bar background.
            '';
            visible = false;
          };

          fg = mkOption {
            type = str;
            default = null;
            description = lib.mdDoc ''
              The color of the active tab bar foreground.
            '';
            visible = false;
          };
        };

        inactiveTab = {
          bg = mkOption {
            type = str;
            default = null;
            description = lib.mdDoc ''
              The color of the inactive tab bar background.
            '';
            visible = false;
          };

          fg = mkOption {
            type = str;
            default = null;
            description = lib.mdDoc ''
              The color of the inactive tab bar foreground.
            '';
            visible = false;
          };
        };
      };

      statusLine = {
        sections =
          genAttrs
            [
              "a"
              "b"
              "c"
              "x"
              "y"
              "z"
            ]
            (
              name:
              mkOption {
                type = statusLineSectionType;
                default = null;
                description = mdDoc ''
                  The colors of the status line section ${toUpper name}.
                '';
                visible = false;
              }
            );
      };
    };
  };
}

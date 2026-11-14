{
  lib,
  config,
  inputs,
  ...
}:

let
  inherit (lib)
    attrNames
    genAttrs
    hasAttr
    mdDoc
    mkOption
    toUpper
    types
    mkIf
    ;
  inherit (types)
    attrsOf
    str
    enum
    submodule
    ;
  inherit (inputs.self.lib.extraTypes) fontType;

  cfg = config.modules.term;

  statusLineSectionType = submodule {
    options = {
      fg = mkOption {
        type = str;
        default = null;
        description = mdDoc "The color of the status line section foreground";
        visible = false;
      };

      bg = mkOption {
        type = str;
        default = null;
        description = mdDoc "The color of the status line section background";
        visible = false;
      };
    };
  };

  # Each colorscheme registers a theme entry whose every field maps to the
  # bundled-theme name shipped by the tool itself. All fields are required
  # (no defaults) so adding a new colorscheme without filling every field
  # fails Nix evaluation — this is the "no-mapping-omission" guarantee.
  colorschemeThemeType = submodule {
    options = {
      bat = mkOption {
        type = str;
        description = mdDoc "bat --list-themes name (e.g. \"TwoDark\")";
      };
      doom-emacs = mkOption {
        type = str;
        description = mdDoc "doom-emacs theme symbol (e.g. \"doom-tokyo-night\")";
      };
      helix = mkOption {
        type = str;
        description = mdDoc "Helix bundled theme name (e.g. \"tokyonight\")";
      };
    };
  };
in
{
  options.modules.term = {
    font = mkOption {
      type = fontType;
      default = {
        name = "monospace";
        size = 12;
      };
      description = mdDoc "Font configuration for terminal emulators";
    };

    colorschemes = {
      active = mkOption {
        type = enum [ "tokyo-night" ];
        default = null;
        description = mdDoc "The name of the active colorscheme";
      };

      themes = mkOption {
        type = attrsOf colorschemeThemeType;
        default = { };
        internal = true;
        description = mdDoc ''
          Per-colorscheme registry of bundled-theme names, keyed by
          colorscheme name. Each entry must populate every field of
          `colorschemeThemeType`; missing fields fail Nix evaluation.
        '';
      };

      colors = {
        fg = mkOption {
          type = str;
          default = null;
          description = mdDoc "The color of the foreground";
          visible = false;
        };

        bg = mkOption {
          type = str;
          default = null;
          description = mdDoc "The color of the background";
          visible = false;
        };

        ansi = {
          black = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the black ansi color";
            visible = false;
          };

          red = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the red ansi color";
            visible = false;
          };

          green = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the green ansi color";
            visible = false;
          };

          yellow = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the yellow ansi color";
            visible = false;
          };

          blue = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the blue ansi color";
            visible = false;
          };

          magenta = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the magenta ansi color";
            visible = false;
          };

          cyan = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the cyan ansi color";
            visible = false;
          };

          white = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the white ansi color";
            visible = false;
          };

          brightBlack = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright black ansi color";
            visible = false;
          };

          brightRed = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright red ansi color";
            visible = false;
          };

          brightGreen = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright green ansi color";
            visible = false;
          };

          brightYellow = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright yellow ansi color";
            visible = false;
          };

          brightBlue = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright blue ansi color";
            visible = false;
          };

          brightMagenta = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright magenta ansi color";
            visible = false;
          };

          brightCyan = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright cyan ansi color";
            visible = false;
          };

          brightWhite = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the bright white ansi color";
            visible = false;
          };
        };

        cursor = {
          fg = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the cursor";
            visible = false;
          };

          bg = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the cursor background";
            visible = false;
          };
        };

        selection = {
          fg = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the selection foreground";
            visible = false;
          };

          bg = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the selection background";
            visible = false;
          };
        };

        paneBorder = {
          default = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the default pane border";
            visible = false;
          };
        };

        tabBar = {
          bg = mkOption {
            type = str;
            default = null;
            description = mdDoc "The color of the tab bar background";
            visible = false;
          };

          activeTab = {
            bg = mkOption {
              type = str;
              default = null;
              description = mdDoc "The color of the active tab bar background";
              visible = false;
            };

            fg = mkOption {
              type = str;
              default = null;
              description = mdDoc "The color of the active tab bar foreground";
              visible = false;
            };
          };

          inactiveTab = {
            bg = mkOption {
              type = str;
              default = null;
              description = mdDoc "The color of the inactive tab bar background";
              visible = false;
            };

            fg = mkOption {
              type = str;
              default = null;
              description = mdDoc "The color of the inactive tab bar foreground";
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
                  description = mdDoc "The colors of the status line section ${toUpper name}";
                  visible = false;
                }
              );
        };
      };
    };
  };

  config = {
    assertions = [
      {
        assertion =
          cfg.colorschemes.active == null || hasAttr cfg.colorschemes.active cfg.colorschemes.themes;
        message = ''
          modules.term.colorschemes.active = "${toString cfg.colorschemes.active}" but no
          modules.term.colorschemes.themes."${toString cfg.colorschemes.active}" entry was registered.
          Registered colorschemes: ${toString (attrNames cfg.colorschemes.themes)}.
        '';
      }
    ];

    home.packages = mkIf (cfg.font.package != null) [ cfg.font.package ];
  };
}

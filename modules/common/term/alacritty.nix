{ config, lib, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.alacritty;
  colors = config.modules.term.colorschemes.colors;
in {
  options.modules.term.alacritty = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.alacritty = {
      enable = true;
      settings = {
        # Any items in the `env` entry below will be added as
        # environment variables. Some entries may override variables
        # set by alacritty itself.
        env = {
          # TERM variable
          #
          # This value is used to set the `$TERM` environment variable for
          # each instance of Alacritty. If it is not present, alacritty will
          # check the local terminfo database and use `alacritty` if it is
          # available, otherwise `xterm-256color` is used.
          TERM = "alacritty";
        };

        scrolling = {
          # Maximum number of lines in the scrollback buffer.
          # Specifying '0' will disable scrolling.
          # history = 10000;
        };

        # Font configuration
        font = {
          # Normal (roman) font face
          normal = {
            # Font family
            #
            # Default:
            #   - (macOS) Menlo
            #   - (Linux/BSD) monospace
            #   - (Windows) Consolas
            family = "Input";

            # The `style` can be specified to pick a specific face.
            # style = "Regular";
          };

          # Point size
          size = 17;
        };

        # Thin stroke font rendering (macOS only)
        #
        # Thin strokes are suitable for retina displays, but for non-retina screens
        # it is recommended to set `use_thin_strokes` to `false`.
        use_thin_strokes = true;

        # If `true`, bold text is drawn using the bright color variants.
        # draw_bold_text_with_bright_colors = false;

        # Colors
        colors = {
          # Default colors
          primary = {
            background = "${colors.background}";
            foreground = "${colors.foreground}";
          };

          # Cursor colors
          #
          # Colors which should be used to draw the terminal cursor.
          #
          # Allowed values are CellForeground/CellBackground, which reference the
          # affected cell, or hexadecimal colors like #ff00ff.
          cursor.cursor = "${colors.cursor}";

          # Selection colors
          #
          # Colors which should be used to draw the selection area.
          #
          # Allowed values are CellForeground/CellBackground, which reference the
          # affected cell, or hexadecimal colors like #ff00ff.
          selection = {
            text = "CellBackground";
            background = "CellForeground";
          };

          inherit (colors) normal bright;
        };
      };
    };
  };
}

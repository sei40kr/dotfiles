{ config, lib, pkgs, ... }:

with lib;
let
  iterm2Config = optionalString config.modules.term.iterm2.enable ''
    # Set the preview image method. Supported methods:
    set preview_images_method iterm2

    # Default iTerm2 font size (see: preview_images_method: iterm2)
    set iterm2_font_width 8
    set iterm2_font_height 11
  '';
in {
  options.modules.shell.tools.ranger.enable = mkOption {
    type = types.bool;
    default = false;
  };

  config = mkIf config.modules.shell.tools.ranger.enable {
    my.packages = with pkgs; [ ranger ];

    my.home.xdg.configFile."ranger/rc.conf".text = ''
      # ===================================================================
      # This file contains the default startup commands for ranger.
      # To change them, it is recommended to create either /etc/ranger/rc.conf
      # (system-wide) or ~/.config/ranger/rc.conf (per user) and add your custom
      # commands there.
      #
      # If you copy this whole file there, you may want to set the environment
      # variable RANGER_LOAD_DEFAULT_RC to FALSE to avoid loading it twice.
      #
      # The purpose of this file is mainly to define keybindings and settings.
      # For running more complex python code, please create a plugin in "plugins/" or
      # a command in "commands.py".
      #
      # Each line is a command that will be run before the user interface
      # is initialized.  As a result, you can not use commands which rely
      # on the UI such as :delete or :mark.
      # ===================================================================

      # ===================================================================
      # == Options
      # ===================================================================

      # How many columns are there, and what are their relative widths?
      set column_ratios 1,1,1

      # Show hidden files? You can toggle this by typing 'zh'
      set show_hidden true

      # Ask for a confirmation when running the "delete" command?
      # Valid values are "always", "never", "multiple" (default)
      # With "multiple", ranger will ask only if you delete multiple files at once.
      set confirm_on_delete always

      ${iterm2Config}

      # Which colorscheme to use?  These colorschemes are available by default:
      # default, jungle, snow, solarized
      set colorscheme default

      # Preview files on the rightmost column?
      # And collapse (shrink) the last column if there is nothing to preview?
      set preview_files true
      set preview_directories true
      set collapse_preview false

      # Draw borders around columns? (separators, outline, both, or none)
      # Separators are vertical lines between columns.
      # Outline draws a box around all the columns.
      # Both combines the two.
      set draw_borders separators

      # Enable the mouse support?
      set mouse_enabled false

      # Show hostname in titlebar?
      set hostname_in_titlebar false

      # Abbreviate $HOME with ~ in the titlebar (first line) of ranger?
      set tilde_in_titlebar true

      # Use fuzzy tab completion with the "cd" command. For example,
      # ":cd /u/lo/b<tab>" expands to ":cd /usr/local/bin".
      set cd_tab_fuzzy true

      # Disable displaying line numbers in main column.
      # Possible values: false, absolute, relative.
      set line_numbers relative

      # Start line numbers from 1 instead of 0
      set one_indexed true
    '';
  };
}

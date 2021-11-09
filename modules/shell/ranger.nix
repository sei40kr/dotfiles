{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.ranger;
in {
  options.modules.shell.ranger = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      ranger

      # Optional dependencies
      ffmpeg
    ];

    environment.etc."ranger/rc.conf".text = ''
      # ===================================================================
      # == Options
      # ===================================================================

      # How many columns are there, and what are their relative widths?
      set column_ratios 1

      # Show hidden files? You can toggle this by typing 'zh'
      set show_hidden true

      # Ask for a confirmation when running the "delete" command?
      # Valid values are "always", "never", "multiple" (default)
      # With "multiple", ranger will ask only if you delete multiple files at once.
      set confirm_on_delete always

      # Use non-default path for file preview script?
      # ranger ships with scope.sh, a script that calls external programs (see
      # README.md for dependencies) to preview images, archives, etc.
      set preview_script ${pkgs.ranger}/lib/python3.9/site-packages/ranger/data/scope.sh

      # Automatically count files in the directory, even before entering them?
      set automatically_count_files false

      # Which colorscheme to use?  These colorschemes are available by default:
      # default, jungle, snow, solarized
      set colorscheme default

      # Preview files on the rightmost column?
      # And collapse (shrink) the last column if there is nothing to preview?
      set preview_files false
      set preview_directories true

      # Draw borders around columns? (separators, outline, both, or none)
      # Separators are vertical lines between columns.
      # Outline draws a box around all the columns.
      # Both combines the two.
      set draw_borders both

      # Enable the mouse support?
      set mouse_enabled false

      # Display the file size in the main column or status bar?
      set display_size_in_main_column false

      # Display files tags in all columns or only in main column?
      set display_tags_in_all_columns false

      # Set a title for the window? Updates both `WM_NAME` and `WM_ICON_NAME`
      set update_title true

      # Show hostname in titlebar?
      set hostname_in_titlebar false

      # Abbreviate $HOME with ~ in the titlebar (first line) of ranger?
      set tilde_in_titlebar true

      # Add the highlighted file to the path in the titlebar
      set show_selection_in_titlebar false

      # Start line numbers from 1 instead of 0
      set one_indexed true
    '';
  };
}

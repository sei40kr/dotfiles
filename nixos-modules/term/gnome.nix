{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.term.gnome;
  termCfg = config.modules.term;

  inherit (termCfg) font;
  inherit (termCfg.theme) colors;
in
{
  options.modules.term.gnome = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ gnome.gnome-terminal ];

    modules.desktop.dconf = {
      enable = true;
      settings = {
        # Disable all shortcuts other than Copy, Paste
        "org/gnome/terminal/legacy/keybindings" = {
          # New Tab
          new-tab = "disabled";
          # New Window
          new-window = "disabled";
          # Close Tab
          close-tab = "disabled";
          # Close Window
          close-window = "disabled";
          # Full Screen
          full-screen = "disabled";
          # Zoom In
          zoom-in = "disabled";
          # Zoom Out
          zoom-out = "disabled";
          # Normal Size
          zoom-normal = "disabled";
          # Find
          find = "disabled";
          # Find Next
          find-next = "disabled";
          # Find Previous
          find-previous = "disabled";
          # Clear Highlight
          find-clear = "disabled";
          # Switch to Previous Tab
          prev-tab = "disabled";
          # Switch to Next Tab
          next-tab = "disabled";
          # Move Tab to the Left
          move-tab-left = "disabled";
          # Move Tab to the Right
          move-tab-right = "disabled";
          # Switch to Tab
          switch-to-tab-1 = "disabled";
          switch-to-tab-2 = "disabled";
          switch-to-tab-3 = "disabled";
          switch-to-tab-4 = "disabled";
          switch-to-tab-5 = "disabled";
          switch-to-tab-6 = "disabled";
          switch-to-tab-7 = "disabled";
          switch-to-tab-8 = "disabled";
          switch-to-tab-9 = "disabled";
          switch-to-tab-10 = "disabled";
        };
        "org/gnome/terminal/legacy/profiles:" = {
          list = [ "b1dcc9dd-5262-4d8d-a863-c897e6d979b9" ];
        };
        "org/gnome/terminal/legacy/profiles:/:b1dcc9dd-5262-4d8d-a863-c897e6d979b9" = {
          visible-name = "Default";
          use-system-font = false;
          font = "${font.name} ${toString font.size}";
          # Disable cursor blinking
          cursor-blink-mode = "off";
          use-theme-colors = false;
          # Default color
          foreground-color = "#${colors.fg}";
          background-color = "#${colors.bg}";
          # Cursor color
          cursor-colors-set = true;
          cursor-foreground-color = "#${colors.cursor.fg}";
          cursor-background-color = "#${colors.cursor.bg}";
          # Highlight color
          highlight-colors-set = true;
          highlight-foreground-color = "#${colors.selection.fg}";
          highlight-background-color = "#${colors.selection.bg}";
          # Palette
          palette = [
            "#${colors.base0}"
            "#${colors.base1}"
            "#${colors.base2}"
            "#${colors.base3}"
            "#${colors.base4}"
            "#${colors.base5}"
            "#${colors.base6}"
            "#${colors.base7}"
            "#${colors.base8}"
            "#${colors.base9}"
            "#${colors.base10}"
            "#${colors.base11}"
            "#${colors.base12}"
            "#${colors.base13}"
            "#${colors.base14}"
            "#${colors.base15}"
          ];
          # Hide scrollbar
          scrollbar-policy = "never";
        };
      };
    };
  };
}

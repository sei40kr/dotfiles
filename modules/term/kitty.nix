{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  inherit (pkgs) stdenv;
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
  inherit (termCfg) font;
  inherit (termCfg.theme) colors;
in
{
  options.modules.term.kitty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ kitty ];

    fonts.packages = with pkgs; [
      (nerdfonts.override { fonts = [ "NerdFontsSymbolsOnly" ]; })
    ];

    home.configFile."kitty/kitty.conf".text = ''
      confirm_os_window_close 0

      ## Fonts
      font_family       ${font.name}
      font_size         ${toString font.size}.0
      # Nerd Fonts v2.3.3
      symbol_map        U+23FB-U+23FE,U+2665,U+26A1,U+2B58,U+E000-U+E00A,U+E0A0-U+E0A3,U+E0B0-U+E0D4,U+E200-U+E2A9,U+E300-U+E3E3,U+E5FA-U+E6AA,U+E700-U+E7C5,U+EA60-U+EBEB,U+F000-U+F2E0,U+F300-U+F32F,U+F400-U+F4A9,U+F500-U+F8FF,U+F0001-U+F1AF0 Symbols Nerd Font Mono
      disable_ligatures always

      ## Cursor customization
      cursor            #${colors.cursor.bg}
      cursor_text_color #${colors.cursor.fg}

      ## Mouse
      url_color #${colors.url}

      ## Window layout
      window_border_width 1px
      window_padding_width 12
      active_border_color #${colors.border.active}
      inactive_border_color #${colors.border.inactive}
      bell_border_color #${colors.border.bell}

      ## Tab bar
      tab_bar_style separator
      tab_separator " "
      tab_title_template " [{index}] {title} "
      active_tab_foreground #${colors.tab.active.fg}
      active_tab_background #${colors.tab.active.bg}
      inactive_tab_foreground #${colors.tab.inactive.fg}
      inactive_tab_background #${colors.tab.inactive.bg}
      tab_bar_background #${colors.tab.bg}

      ## Color scheme
      foreground #${colors.fg}
      background #${colors.bg}
      selection_foreground #${colors.selection.fg}
      selection_background #${colors.selection.bg}

      # The color table
      color0  #${colors.base0}
      color1  #${colors.base1}
      color2  #${colors.base2}
      color3  #${colors.base3}
      color4  #${colors.base4}
      color5  #${colors.base5}
      color6  #${colors.base6}
      color7  #${colors.base7}
      color8  #${colors.base8}
      color9  #${colors.base9}
      color10 #${colors.base10}
      color11 #${colors.base11}
      color12 #${colors.base12}
      color13 #${colors.base13}
      color14 #${colors.base14}
      color15 #${colors.base15}

      ## Advanced
      ${optionalString stdenv.isDarwin ''
        env LANG=en_US.UTF-8
      ''}
      # A workaround for a bug where it appends to the clipboard each time text
      # is copied rather than replacing it.
      # See https://github.com/tmux/tmux/wiki/Clipboard#terminal-support---kitty
      clipboard_control write-primary write-clipboard no-append

      ## OS specific tweaks
      macos_option_as_alt yes

      ## Keyboard shortcuts
      clear_all_shortcuts no
    '';
  };
}

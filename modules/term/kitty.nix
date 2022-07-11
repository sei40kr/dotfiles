{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
in
{
  options.modules.term.kitty = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ kitty ];

    home.configFile."kitty/kitty.conf".text = ''
      ## Fonts
      font_family       ${termCfg.font.name}
      font_size         ${toString termCfg.font.size}.0
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

      ## OS specific tweaks
      macos_option_as_alt yes

      ## Keyboard shortcuts
      clear_all_shortcuts no
    '';
  };
}

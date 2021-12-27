{ config, lib, ... }:

with builtins;
with lib;
with lib.my;
let
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
in {
  options.modules.term.kitty = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    home-manager.users.${config.user.name}.programs.kitty = {
      enable = true;
      font = termCfg.font;
      keybindings = let
        keys = [
          "ctrl+shift+up"
          "ctrl+shift+down"
          "ctrl+shift+page_up"
          "ctrl+shift+page_down"
          "ctrl+shift+home"
          "ctrl+shift+end"
          "ctrl+shift+t"
          "ctrl+shift+q"
          "ctrl+shift+right"
          "ctrl+shift+left"
          "ctrl+shift+l"
          "ctrl+shift+."
          "ctrl+shift+,"
          "ctrl+shift+alt+t"
          "ctrl+shift+enter"
          "ctrl+shift+n"
          "ctrl+shift+w"
          "ctrl+shift+]"
          "ctrl+shift+["
          "ctrl+shift+f"
          "ctrl+shift+b"
          "ctrl+shift+`"
          "ctrl+shift+1"
          "ctrl+shift+2"
          "ctrl+shift+3"
          "ctrl+shift+4"
          "ctrl+shift+5"
          "ctrl+shift+6"
          "ctrl+shift+7"
          "ctrl+shift+8"
          "ctrl+shift+9"
          "ctrl+shift+0"
          "ctrl+shift+c"
          "ctrl+shift+v"
          "ctrl+shift+s"
          "ctrl+shift+equal"
          "ctrl+shift+minus"
          "ctrl+shift+backspace"
          "ctrl+shift+f11"
          "ctrl+shift+f10"
          "ctrl+shift+u"
          "ctrl+shift+e"
          "ctrl+shift+delete"
          "ctrl+shift+f5"
          "ctrl+shift+f6"
          "ctrl+shift+o"
          "ctrl+shift+f2"
          "ctrl+shift+escape"
          "ctrl+shift+a>m"
          "ctrl+shift+a>l"
          "ctrl+shift+a>1"
          "ctrl+shift+a>d"
        ];
      in listToAttrs (map (name: {
        inherit name;
        value = "";
      }) keys);
      extraConfig = let inherit (termCfg.theme) colors;
      in ''
        ## Fonts
        adjust_column_width -1

        ## Cursor customization
        cursor            #${colors.cursor.bg}
        cursor_text_color #${colors.cursor.fg}

        ## Mouse
        url_color #${colors.url}

        ## Window layout
        window_padding_width 12

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
      '';
    };
  };
}

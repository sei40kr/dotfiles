{ config, lib, ... }:

with builtins;
with lib;
with lib.my;
let
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
  colors = termCfg.theme.colors;
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
      extraConfig = let
        inherit (colors) fg bg black red green yellow blue magenta cyan white;
      in ''
        ## Fonts
        adjust_column_width -1

        ## Window layout
        window_padding_width 12

        ## Color scheme
        foreground ${fg}
        background ${bg}
        color0  ${black}
        color8  ${black}
        color1  ${red}
        color9  ${red}
        color2  ${green}
        color10 ${green}
        color3  ${yellow}
        color11 ${yellow}
        color4  ${blue}
        color12 ${blue}
        color5  ${magenta}
        color13 ${magenta}
        color6  ${cyan}
        color14 ${cyan}
        color7  ${white}
        color15 ${white}

        ## OS specific tweaks
        macos_option_as_alt yes
      '';
    };
  };
}

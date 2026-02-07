{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) toString;
  inherit (lib) mkEnableOption mkIf;
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
  inherit (termCfg) font;
  inherit (termCfg.colorschemes.colors)
    fg
    bg
    ansi
    cursor
    link
    selection
    paneBorder
    tabBar
    ;
in
{
  options.modules.term.kitty = {
    enable = mkEnableOption "kitty";
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      settings = {
        confirm_os_window_close = 0;

        # Nerd Fonts v2.3.3
        symbol_map = "U+23FB-U+23FE,U+2665,U+26A1,U+2B58,U+E000-U+E00A,U+E0A0-U+E0A3,U+E0B0-U+E0D4,U+E200-U+E2A9,U+E300-U+E3E3,U+E5FA-U+E6AA,U+E700-U+E7C5,U+EA60-U+EBEB,U+F000-U+F2E0,U+F300-U+F32F,U+F400-U+F4A9,U+F500-U+F8FF,U+F0001-U+F1AF0 Symbols Nerd Font Mono";
        disable_ligatures = "always";

        # Cursor customization
        cursor = "#${cursor.bg}";
        cursor_text_color = "#${cursor.fg}";

        # Mouse
        url_color = "#${link}";

        # Window layout
        window_border_width = "1px";
        window_padding_width = 12;
        active_border_color = "#${paneBorder.focused}";
        inactive_border_color = "#${paneBorder.default}";
        bell_border_color = "#${paneBorder.urgent}";

        # Tab bar
        tab_bar_style = "separator";
        tab_separator = ''" "'';
        tab_title_template = ''" [{index}] {title} "'';
        active_tab_foreground = "#${tabBar.activeTab.fg}";
        active_tab_background = "#${tabBar.activeTab.bg}";
        inactive_tab_foreground = "#${tabBar.inactiveTab.fg}";
        inactive_tab_background = "#${tabBar.inactiveTab.bg}";
        tab_bar_background = "#${tabBar.bg}";

        # Color scheme
        foreground = "#${fg}";
        background = "#${bg}";
        background_opacity = toString termCfg.bgOpacity;
        background_blur = toString termCfg.bgBlur;
        selection_foreground = "#${selection.fg}";
        selection_background = "#${selection.bg}";

        # The color table
        color0 = "#${ansi.black}";
        color1 = "#${ansi.red}";
        color2 = "#${ansi.green}";
        color3 = "#${ansi.yellow}";
        color4 = "#${ansi.blue}";
        color5 = "#${ansi.magenta}";
        color6 = "#${ansi.cyan}";
        color7 = "#${ansi.white}";
        color8 = "#${ansi.brightBlack}";
        color9 = "#${ansi.brightRed}";
        color10 = "#${ansi.brightGreen}";
        color11 = "#${ansi.brightYellow}";
        color12 = "#${ansi.brightBlue}";
        color13 = "#${ansi.brightMagenta}";
        color14 = "#${ansi.brightCyan}";
        color15 = "#${ansi.brightWhite}";

        # Advanced
        # A workaround for a bug where it appends to the clipboard each time text
        # is copied rather than replacing it.
        # See https://github.com/tmux/tmux/wiki/Clipboard#terminal-support---kitty
        clipboard_control = "write-primary write-clipboard no-append";

        # OS specific tweaks
        macos_option_as_alt = "yes";

        # Keyboard shortcuts
        clear_all_shortcuts = false;
      };
      font = { inherit (font) name size; };
    };

    home.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) toString;
  inherit (lib) mkEnableOption mkIf;
  termCfg = config.modules.term;
  cfg = termCfg.kitty;
in
{
  options.modules.term.kitty = {
    enable = mkEnableOption "kitty";
  };

  config = mkIf cfg.enable {
    programs.kitty = {
      enable = true;
      themeFile = termCfg.colorschemes.themes.${termCfg.colorschemes.active}.kitty;
      settings = {
        confirm_os_window_close = 0;

        # Nerd Fonts v2.3.3
        symbol_map = "U+23FB-U+23FE,U+2665,U+26A1,U+2B58,U+E000-U+E00A,U+E0A0-U+E0A3,U+E0B0-U+E0D4,U+E200-U+E2A9,U+E300-U+E3E3,U+E5FA-U+E6AA,U+E700-U+E7C5,U+EA60-U+EBEB,U+F000-U+F2E0,U+F300-U+F32F,U+F400-U+F4A9,U+F500-U+F8FF,U+F0001-U+F1AF0 Symbols Nerd Font Mono";
        disable_ligatures = "always";

        # Window layout
        window_border_width = "1px";
        window_padding_width = 12;

        # Tab bar
        tab_bar_style = "separator";
        tab_separator = ''" "'';
        tab_title_template = ''" [{index}] {title} "'';

        background_opacity = toString termCfg.bgOpacity;
        background_blur = toString termCfg.bgBlur;

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
      font = { inherit (termCfg.font) name size; };
    };

    home.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}

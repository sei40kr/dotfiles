{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (builtins) toString;
  inherit (lib) mkEnableOption mkIf;
  termCfg = config.modules.term;
  cfg = termCfg.ghostty;
  inherit (termCfg) font;
  inherit (termCfg.colorschemes.colors)
    fg
    bg
    ansi
    cursor
    selection
    ;

  ghostty-tmux = perSystem.ghostty-tmux.default.override { prefix = "ctrl+t"; };
in
{
  options.modules.term.ghostty = {
    enable = mkEnableOption "Ghostty";
  };

  config = mkIf cfg.enable {
    programs.ghostty = {
      enable = true;
      package = pkgs.ghostty-bin;
      settings = {
        font-family = font.name;
        font-size = font.size;

        background-opacity = termCfg.bgOpacity;
        background-blur-radius = termCfg.bgBlur;

        cursor-color = "#${cursor.bg}";
        cursor-text = "#${cursor.fg}";

        selection-foreground = "#${selection.fg}";
        selection-background = "#${selection.bg}";

        foreground = "#${fg}";
        background = "#${bg}";

        palette = [
          "0=#${ansi.black}"
          "1=#${ansi.red}"
          "2=#${ansi.green}"
          "3=#${ansi.yellow}"
          "4=#${ansi.blue}"
          "5=#${ansi.magenta}"
          "6=#${ansi.cyan}"
          "7=#${ansi.white}"
          "8=#${ansi.brightBlack}"
          "9=#${ansi.brightRed}"
          "10=#${ansi.brightGreen}"
          "11=#${ansi.brightYellow}"
          "12=#${ansi.brightBlue}"
          "13=#${ansi.brightMagenta}"
          "14=#${ansi.brightCyan}"
          "15=#${ansi.brightWhite}"
        ];

        # HACK: Ghostty uses the fixterms spec by default, encoding ctrl+key
        # combos as CSI sequences (e.g. ctrl+[ becomes CSI 91;5u instead of
        # ^[). Programs that don't activate the Kitty Keyboard Protocol can't
        # understand these, breaking vi-mode shell navigation.
        # See: https://github.com/ghostty-org/ghostty/discussions/5071
        keybind = [
          "ctrl+m=text:\\r"
          "ctrl+i=text:\\x09"
          "ctrl+[=text:\\x1b"
        ];

        window-padding-x = 12;
        window-padding-y = 12;

        macos-option-as-alt = true;

        config-file = [
          "${ghostty-tmux}/share/ghostty-tmux/tmux"
          "${ghostty-tmux}/share/ghostty-tmux/pain-control"
        ];
      };
    };

    home.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}

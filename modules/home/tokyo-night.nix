{
  config,
  lib,
  ...
}:

let
  inherit (builtins) substring;
  inherit (lib) fromHexString mkIf;

  colors = {
    fg = "c0caf5";
    bg = "1a1b26";

    ansi = {
      black = "1a1b26";
      red = "f7768e";
      green = "9ece6a";
      yellow = "e0af68";
      blue = "7aa2f7";
      magenta = "bb9af7";
      cyan = "7dcfff";
      white = "a9b1d6";
      brightBlack = "414868";
      brightRed = "f7768e";
      brightGreen = "9ece6a";
      brightYellow = "e0af68";
      brightBlue = "7aa2f7";
      brightMagenta = "bb9af7";
      brightCyan = "7dcfff";
      brightWhite = "c0caf5";
    };

    cursor = {
      fg = "1a1b26";
      bg = "c0caf5";
    };

    selection = {
      fg = "c0caf5";
      bg = "33467c";
    };

    paneBorder = {
      default = "3b4261";
    };

    tabBar = {
      bg = "15161e";

      activeTab = {
        bg = "7aa2f7";
        fg = "1f2335";
      };

      inactiveTab = {
        bg = "292e42";
        fg = "545c7e";
      };
    };

    statusLine = {
      sections = rec {
        a = {
          fg = "15161e";
          bg = "7aa2f7";
        };

        b = {
          fg = "7aa2f7";
          bg = "3b4261";
        };

        c = {
          fg = "a9b1d6";
          bg = "16161e";
        };

        x = c;
        y = b;
        z = a;
      };
    };
  };

  srgb = hex: {
    colorSpace = "srgb";
    red = fromHexString (substring 0 2 hex) / 255.0;
    green = fromHexString (substring 2 2 hex) / 255.0;
    blue = fromHexString (substring 4 2 hex) / 255.0;
    alpha = 1;
  };
in
{
  config = {
    modules.term.colorschemes.themes.tokyo-night = {
      bat = "TwoDark";
      doom-emacs = "doom-tokyo-night";
      helix = "tokyonight";
    };

    modules.term.rex.themes.tokyo-night = {
      id = "custom-5F1E3A2C-7B4D-4E8A-9C6F-0D2B8A4E1F37";
      name = "Tokyo Night";
      appearance = "dark";
      createdAt = 0;
      source.ghosttyFile.fileName = "tokyo-night";
      palette = {
        background = srgb colors.bg;
        foreground = srgb colors.fg;
        cursor = srgb colors.cursor.bg;
        cursorText = srgb colors.cursor.fg;
        selectionBackground = srgb colors.selection.bg;
        selectionForeground = srgb colors.selection.fg;
        ansi = map srgb (
          with colors.ansi;
          [
            black
            red
            green
            yellow
            blue
            magenta
            cyan
            white
            brightBlack
            brightRed
            brightGreen
            brightYellow
            brightBlue
            brightMagenta
            brightCyan
            brightWhite
          ]
        );
        paletteGenerate = false;
        paletteHarmonious = false;
        paletteBase = { };
        paletteOverrides = { };
      };
    };

    modules.term.colorschemes.colors = mkIf (
      config.modules.term.colorschemes.active == "tokyo-night"
    ) colors;
  };
}

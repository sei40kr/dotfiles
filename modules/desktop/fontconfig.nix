{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf types;
  inherit (lib.my) mkBoolOpt mkOpt;
  inherit (types)
    listOf
    package
    str
    submodule
    ;
  cfg = config.modules.desktop.fontconfig;
  fontType = submodule {
    options = {
      packages = mkOpt (listOf package) [ ];
      names = mkOpt (listOf str) [ ];
    };
  };

  fontPackages =
    cfg.fonts.sansSerif.packages
    ++ cfg.fonts.serif.packages
    ++ cfg.fonts.monospace.packages
    ++ cfg.fonts.emoji.packages;
in
{
  options.modules.desktop.fontconfig = {
    enable = mkBoolOpt false;

    fonts = {
      sansSerif = mkOpt fontType {
        packages = with pkgs; [
          noto-fonts
          noto-fonts-cjk-sans
        ];
        names = [
          "Noto Sans"
          "Noto Sans CJK JP"
        ];
      };
      serif = mkOpt fontType {
        packages = with pkgs; [
          noto-fonts
          noto-fonts-cjk-serif
        ];
        names = [
          "Noto Serif"
          "Noto Serif CJK JP"
        ];
      };
      monospace = mkOpt fontType {
        packages = with pkgs; [
          jetbrains-mono
          noto-fonts
          noto-fonts-cjk-sans
          powerline-symbols
        ];
        names = [
          "JetBrains Mono"
          "Noto Sans Mono"
          "Noto Sans Mono CJK JP"
          "PowerlineSymbols"
        ];
      };
      emoji = mkOpt fontType {
        packages = with pkgs; [ noto-fonts-emoji ];
        names = [ "Noto Color Emoji" ];
      };
    };
  };

  config = mkIf cfg.enable {
    fonts = {
      fontconfig = {
        enable = true;
        defaultFonts = {
          sansSerif = cfg.fonts.sansSerif.names;
          serif = cfg.fonts.serif.names;
          monospace = cfg.fonts.monospace.names;
          emoji = cfg.fonts.emoji.names;
        };
        hinting.enable = false;
      };

      packages = fontPackages;
    };
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  inherit (types)
    listOf
    package
    str
    submodule
    ;
  cfg = config.modules.desktop.fontconfig;

  fontType = submodule {
    options = {
      packages = mkOption {
        type = listOf package;
        default = [ ];
        description = "Font packages to install.";
      };
      names = mkOption {
        type = listOf str;
        default = [ ];
        description = "Font names for fontconfig.";
      };
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
    enable = mkEnableOption "Fontconfig";

    fonts = {
      sansSerif = mkOption {
        type = fontType;
        default = {
          packages = with pkgs; [
            noto-fonts
            noto-fonts-cjk-sans
          ];
          names = [
            "Noto Sans"
            "Noto Sans CJK JP"
          ];
        };
        description = "Sans-serif fonts.";
      };

      serif = mkOption {
        type = fontType;
        default = {
          packages = with pkgs; [
            noto-fonts
            noto-fonts-cjk-serif
          ];
          names = [
            "Noto Serif"
            "Noto Serif CJK JP"
          ];
        };
        description = "Serif fonts.";
      };

      monospace = mkOption {
        type = fontType;
        default = {
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
        description = "Monospace fonts.";
      };

      emoji = mkOption {
        type = fontType;
        default = {
          packages = with pkgs; [ noto-fonts-color-emoji ];
          names = [ "Noto Color Emoji" ];
        };
        description = "Emoji fonts.";
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

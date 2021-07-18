{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.fontconfig;
  fontType = with types;
    submodule {
      options = {
        packages = mkOpt (listOf package) [ ];
        names = mkOpt (listOf str) [ ];
      };
    };

  fontPackages = cfg.fonts.sansSerif.packages ++ cfg.fonts.serif.packages
    ++ cfg.fonts.monospace.packages ++ cfg.fonts.emoji.packages;
in {
  options.modules.desktop.fontconfig = {
    enable = mkBoolOpt false;
    fonts = {
      sansSerif = mkOpt fontType {
        packages = with pkgs; [ noto-fonts noto-fonts-cjk ];
        names = [ "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
      };
      serif = mkOpt fontType {
        packages = with pkgs; [ noto-fonts noto-fonts-cjk ];
        names = [ "Noto Serif" "Noto Serif CJK JP" ];
      };
      monospace = mkOpt fontType {
        packages = with pkgs; [ jetbrains-mono noto-fonts noto-fonts-cjk ];
        names = [ "JetBrains Mono" "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
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

      fonts = fontPackages;
    };

    modules.desktop.dconf = mkIf config.modules.desktop.gnome.enable {
      enable = true;
      settings."org/gnome/desktop/interface" = {
        font-antialiasing = "rgba";
        font-hinting = "none";
        font-rgba-order = "rgb";
      };
    };
  };
}

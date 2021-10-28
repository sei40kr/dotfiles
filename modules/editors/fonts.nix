{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.fonts;
in {
  options.modules.editors.fonts = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    fonts.fonts = with pkgs; [
      anonymousPro
      fantasque-sans-mono
      fira-code
      hack-font
      hasklig
      inconsolata
      # input-fonts
      iosevka
      jetbrains-mono
      source-code-pro
      source-han-code-jp
      ubuntu_font_family
      victor-mono
    ];
  };
}

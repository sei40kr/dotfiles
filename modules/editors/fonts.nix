{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;

  cfg = config.modules.editors.fonts;
in
{
  options.modules.editors.fonts = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    fonts.packages = with pkgs; [
      anonymousPro
      fantasque-sans-mono
      fira-code
      hack-font
      hasklig
      inconsolata
      # input-fonts
      jetbrains-mono
      source-code-pro
      source-han-code-jp
      ubuntu-classic
      victor-mono
    ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.media.inkscape;
in
{
  options.modules.desktop.media.inkscape = {
    enable = mkEnableOption "Inkscape";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.inkscape ];
  };
}

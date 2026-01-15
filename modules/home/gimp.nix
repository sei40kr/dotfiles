{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.media.gimp;
in
{
  options.modules.desktop.media.gimp = {
    enable = mkEnableOption "GIMP";
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.gimp ];
  };
}

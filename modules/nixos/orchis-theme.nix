{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;

  themeCfg = config.modules.desktop.theme;

  orchis-theme = pkgs.orchis-theme.override { withWallpapers = true; };
in
{
  config = mkIf (themeCfg.active == "orchis") {
    modules.desktop.de.background.image = {
      path = "${orchis-theme}/share/backgrounds/4k.jpg";
      mode = "fill";
    };
  };
}

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
  imports = [
    inputs.self.nixosModules.de
    inputs.self.nixosModules.dunst
    inputs.self.nixosModules.theme-shared
  ];

  config = mkIf (themeCfg.active == "orchis") {
    modules.desktop.de.background.image = {
      path = "${orchis-theme}/share/backgrounds/4k.jpg";
      mode = "fill";
    };

    modules.desktop.apps.dunst = {
      padding = {
        x = 18;
        y = 8;
        textIcon = 8;
      };

      borderWidth = 0;

      icon.size = {
        min = 48;
        max = 48;
      };

      cornerRadius = 18;

      normal = {
        background = "#f5f5f5";
        foreground = "#000000de";
      };
    };
  };
}

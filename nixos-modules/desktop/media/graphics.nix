{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.graphics;
in {
  options.modules.desktop.media.graphics = {
    enable = mkBoolOpt false;
    vector.enable = mkBoolOpt true;
    raster.enable = mkBoolOpt true;
    tools.enable = mkBoolOpt true;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs;
      (optionals cfg.vector.enable [ inkscape ])
      ++ (optionals cfg.raster.enable [ gimp krita ])
      ++ (optionals cfg.tools.enable [ imagemagick ]);
  };
}

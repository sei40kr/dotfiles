{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.media.video;
in
{
  options.modules.desktop.media.video = {
    vlc.enable = mkBoolOpt true;

    handbrake.enable = mkEnableOption "HandBrake";
  };

  config = {
    user.packages = with pkgs; [
      (mkIf cfg.vlc.enable vlc)
      (mkIf cfg.handbrake.enable handbrake)
    ];
  };
}

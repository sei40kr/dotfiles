{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  inherit (lib.my) mkBoolOpt;
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

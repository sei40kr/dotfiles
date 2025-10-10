{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
  inherit (lib.my) mkBoolOpt;
  cfg = config.modules.desktop.media.recording;
in
{
  options.modules.desktop.media.recording = {
    enable = mkBoolOpt false;

    video.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable { user.packages = with pkgs; [ (mkIf cfg.video.enable obs-studio) ]; };
}

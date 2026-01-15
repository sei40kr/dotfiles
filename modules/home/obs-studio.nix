{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.media.obs-studio;
in
{
  options.modules.desktop.media.obs-studio = {
    enable = mkEnableOption "OBS Studio";
  };

  config = mkIf cfg.enable {
    programs.obs-studio.enable = true;
  };
}

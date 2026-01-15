{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.shell.atuin;
in
{
  options.modules.shell.atuin = {
    enable = mkEnableOption "Atuin";
  };

  config = mkIf cfg.enable {
    programs.atuin = {
      enable = true;
      settings = {
        dialect = "us";
        style = "compact";
        store_failed = false;
      };
    };
  };
}

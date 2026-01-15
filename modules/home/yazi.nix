{
  config,
  lib,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.shell.yazi;
in
{
  options.modules.shell.yazi = {
    enable = mkEnableOption "Yazi";
  };

  config = mkIf cfg.enable {
    programs.yazi = {
      enable = true;
      settings = {
        manager.show_hidden = true;
      };
    };
  };
}

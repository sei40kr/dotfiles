{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;

  cfg = config.modules.shell.apps.fastfetch;

  config_jsonc = pkgs.replaceVars ../../../config/fastfetch/config.jsonc {
    logo = "${../../../config/fastfetch/logo.png}";
  };
in
{
  options.modules.shell.apps.fastfetch = {
    enable = mkEnableOption "fastfetch";
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ fastfetch ];

    home.configFile."fastfetch/config.jsonc".source = config_jsonc;
  };
}

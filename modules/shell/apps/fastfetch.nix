{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf mkEnableOption;
  inherit (config.dotfiles) configDir;
  cfg = config.modules.shell.apps.fastfetch;

  config_jsonc = pkgs.substituteAll {
    src = ../../../config/fastfetch/config.jsonc;
    logo = "${configDir}/fastfetch/logo.png";
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

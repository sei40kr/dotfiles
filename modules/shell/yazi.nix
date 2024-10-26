{
  config,
  lib,
  pkgs,
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
    environment.systemPackages = with pkgs; [ yazi ];

    home.configFile."yazi/yazi.toml".text = ''
      [manager]
      show_hidden = true
    '';
  };
}

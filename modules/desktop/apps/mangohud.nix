{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    ;
  cfg = config.modules.desktop.apps.mangohud;
in
{
  options.modules.desktop.apps.mangohud = {
    enable = mkEnableOption "MangoHud";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ mangohud ];

    home.configFile."MangoHud/MangoHud.conf".source = ../../../config/mangohud/MangoHud.conf;
  };
}

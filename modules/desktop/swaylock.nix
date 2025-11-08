{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf escapeShellArg;
  inherit (lib.my) mkBoolOpt;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.swaylock;

  backgroundOpts =
    if deCfg.background.image != null then
      ''
        image=${deCfg.blurredBackgroundImage}
        scaling=${deCfg.background.image.mode}
        color=${deCfg.background.color}
      ''
    else
      ''
        color=${deCfg.background.color}
      '';
  lockCommand = "${pkgs.swaylock-effects}/bin/swaylock -f";
in
{
  options.modules.desktop.swaylock = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ swaylock-effects ];

    home.configFile."swaylock/config".source = pkgs.replaceVars ../../config/swaylock-effects/config {
      inherit backgroundOpts;
      fontName = deCfg.defaultFonts.ui.name;
    };

    security.pam.services.swaylock = { };

    systemd.user.services.swayidle = {
      description = "Swayidle to trigger idle actions";
      documentation = [ "man:swayidle(1)" ];
      serviceConfig = {
        ExecStart =
          "${pkgs.swayidle}/bin/swayidle -w "
          + "timeout 300 ${escapeShellArg lockCommand} "
          + "timeout 600 'swaymsg \"output * dpms off\"' "
          + "resume 'swaymsg \"output * dpms on\"' "
          + "before-sleep ${escapeShellArg lockCommand}";
      };
    };
  };
}

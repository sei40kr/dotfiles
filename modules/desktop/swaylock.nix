{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (builtins) baseNameOf;
  inherit (lib) mkIf escapeShellArg strings;
  inherit (lib.my) mkBoolOpt;
  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.swaylock;

  blurImage =
    path:
    let
      drvName = strings.sanitizeDerivationName "${baseNameOf path}-blurred";
    in
    pkgs.runCommand drvName
      {
        preferLocalBuild = true;
        allowSubstitutes = false;
        buildInputs = with pkgs; [ imagemagick ];
      }
      # Same blur & modulate parameters as the GNOME unlock dialog
      # https://gitlab.gnome.org/GNOME/gnome-shell/-/blob/main/js/ui/unlockDialog.js#L27-28
      ''
        convert ${path} -blur 0x60 -modulate 55,100,100 $out
      '';
  backgroundOpts =
    if deCfg.background.image != null then
      ''
        image=${blurImage deCfg.background.image.path}
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

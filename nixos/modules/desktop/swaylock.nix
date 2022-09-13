{ config, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  desktopCfg = config.modules.desktop;
  cfg = desktopCfg.swaylock;
  inherit (desktopCfg) background fonts;

  blurImage = path:
    let drvName = strings.sanitizeDerivationName "${baseNameOf path}-blurred";
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
    if background.image != null then ''
      image=${blurImage background.image.path}
      scaling=${background.image.mode}
      color=${background.color}
    '' else ''
      color=${background.color}
    '';
  lockCommand = "${pkgs.swaylock-effects}/bin/swaylock -f";
in
{
  options.modules.desktop.swaylock = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ swaylock-effects ];

    home.configFile."swaylock/config".source = pkgs.substituteAll {
      src = ../../../config/swaylock-effects/config;
      inherit backgroundOpts;
      fontName = fonts.ui.name;
    };

    security.pam.services.swaylock = { };

    systemd.user.services.swayidle = {
      description = "Swayidle to trigger idle actions";
      documentation = [ "man:swayidle(1)" ];
      partOf = [ "sway-session.target" ];
      wantedBy = [ "sway-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.swayidle}/bin/swayidle -w " +
          "timeout 300 ${escapeShellArg lockCommand} " +
          "timeout 600 'swaymsg \"output * dpms off\"' " +
          "resume 'swaymsg \"output * dpms on\"' " +
          "before-sleep ${escapeShellArg lockCommand}";
      };
    };
  };
}

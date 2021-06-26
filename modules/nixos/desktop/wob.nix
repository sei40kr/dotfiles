{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.wob;
  wrapper = pkgs.writeShellScriptBin "wob" ''
    ${cfg.package}/bin/wob -W 260 -H 32 -o 10 -b 0 -p 0 --background-color '#bf000000' "$@"
  '';

  wob-pa-inc-vol = pkgs.writeShellScriptBin "wob-pa-inc-vol" ''
    wobsock=$1
    vol=$(${pkgs.pamixer}/bin/pamixer --get-volume)
    vol=$((vol + 5))
    # Limit Pulseaudio max volume
    vol=$((vol > 100 ? 100 : vol))
    ${pkgs.pamixer}/bin/pamixer -u --set-volume $vol && echo $vol >$wobsock
  '';
  wob-pa-dec-vol = pkgs.writeShellScriptBin "wob-pa-dec-vol" ''
    wobsock=$1
    ${pkgs.pamixer}/bin/pamixer -ud 5 && ${pkgs.pamixer}/bin/pamixer --get-volume >$wobsock
  '';
  wob-pa-toggle-mute = pkgs.writeShellScriptBin "wob-pa-toggle-mute" ''
    wobsock=$1
    ${pkgs.pamixer}/bin/pamixer --toggle-mute
    if ${pkgs.pamixer}/bin/pamixer --get-mute; then
      echo 0 >$wobsock
    else
      ${pkgs.pamixer}/bin/pamixer --get-volume >$wobsock
    fi
  '';
in {
  options.modules.desktop.wob = with types; {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = pkgs.wob;
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message = "The wob module requires 'modules.desktop.sway.enable = true'.";
    }];

    home-manager.users.${config.user.name}.wayland.windowManager.sway = {
      extraConfig = ''

        set $WOBSOCK ''${XDG_RUNTIME_DIR:-/run/user/$UID}/wob.sock
        exec ${pkgs.coreutils}/bin/mkfifo $WOBSOCK
        exec ${pkgs.coreutils}/bin/tail -f $WOBSOCK | ${wrapper}/bin/wob
        bindsym XF86AudioRaiseVolume exec ${wob-pa-inc-vol}/bin/wob-pa-inc-vol $WOBSOCK
        bindsym XF86AudioLowerVolume exec ${wob-pa-dec-vol}/bin/wob-pa-dec-vol $WOBSOCK
        bindsym XF86AudioMute exec ${wob-pa-toggle-mute}/bin/wob-pa-toggle-mute $WOBSOCK
      '';
    };
  };
}

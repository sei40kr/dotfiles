{ config, home-manager, lib, pkgs, ... }:

with builtins;
with lib;
with lib.my;
let
  cfg = config.modules.services.random-background;
  sway = config.modules.desktop.sway.package;
  swaybg-random = pkgs.writeShellScriptBin "swaybg-random" ''
    pid="$(${pkgs.procps}/bin/pgrep -x sway)"
    SWAYSOCK="''${XDG_RUNTIME_DIR:-/run/user/''${UID}}/sway-ipc.''${UID}.''${pid}.sock"
    if [[ -S "$SWAYSOCK" ]]; then
      background="$(${pkgs.findutils}/bin/find ${
        escapeShellArg (toString cfg.imageDirectory)
      } \
        -type f \( -name '*.jpg' -o -name '*.png' \) |
        ${pkgs.coreutils}/bin/shuf -n1)"
      ${sway}/bin/swaymsg -s "$SWAYSOCK" "output * bg ''${background} fill"
    fi
  '';
in {
  options.modules.services.random-background = with types; {
    enable = mkBoolOpt false;
    imageDirectory = mkOpt (either path str) null;
  };

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.sway.enable;
      message =
        "The swaybg module requires 'modules.desktop.sway.enable = true'.";
    }];

    home-manager.users.${config.user.name}.systemd.user = {
      services.random-background = {
        Unit = {
          After = [ "sway-session.target" ];
          Description = "Set random desktop background using swaybg";
          PartOf = [ "sway-session.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart = "${swaybg-random}/bin/swaybg-random";
          IOSchedulingClass = "idle";
        };
        Install.WantedBy = [ "sway-session.target" ];
      };
      timers.random-background = {
        Unit.Description = "Set random desktop background using swaybg";
        Timer.OnUnitActiveSec = "1h";
        Install.WantedBy = [ "timers.target" ];
      };
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.editors.emacs;

  emacs = cfg.package;
  emacsclient = pkgs.writeShellScriptBin "emacsclient" ''
    contain_arg() {
      local pattern="$1"
      shift
      local -a args=("$@")
      local not_found=1

      for arg in $args; do
        if [[ "$arg" == "$pattern" ]]; then
          not_found=0
          break
        fi
      done

      return "$not_found"
    }

    if [[ -n "$DISPLAY" ]] && ! contain_arg '-nw' "$@" &&
      ! contain_arg '-t' "$@" && ! contain_arg '--tty' "$@"; then
      frames="$(emacsclient -e "(delete t (mapcar #'framep (frame-list)))" 2>/dev/null)"

      if [[ -n "$frames" && "$frames" != 'nil' ]]; then
        exec ${emacs}/bin/emacsclient -c "$@" 2>/dev/null
      fi
    fi

    exec ${emacs}/bin/emacsclient -s tty "$@"
  '';
in {
  config = mkIf cfg.enable {
    systemd.user.services.emacs-tty = {
      description = "Emacs: the extensible, self-documenting text editor";
      serviceConfig = {
        Type = "simple";
        ExecStart =
          "${pkgs.bash}/bin/bash -c '. ${config.system.build.setEnvironment}; exec ${emacs}/bin/emacs --fg-daemon=tty -nw'";
        Restart = "always";
      };
    };

    modules.shell.aliases = {
      emacs = "${emacsclient}/bin/emacsclient";
      te = "emacs -nw";
    };
  };
}

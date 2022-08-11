{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.shell.tmux;
in
{
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;

    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      pkgs.my.yonmux
      pkgs.tmuxinator
      # VTE terminals (ex. GNOME Terminal) does not support "Ms" capability.
      # See https://github.com/tmux/tmux/wiki/Clipboard#terminal-support---vte-terminals
      (mkIf pkgs.stdenv.isLinux pkgs.xclip)
    ];

    environment.variables = {
      # Store the tmux sockets under /run, which is more secure than /tmp
      TMUX_TMPDIR = optional pkgs.stdenv.isLinux ''''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}'';
    };

    modules.shell.aliases.mux = "tmuxinator";
  };
}

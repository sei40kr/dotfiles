{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.shell.tmux;
in
{
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;

    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      my.yonmux
      tmuxinator
    ];

    environment.variables = {
      # Store the tmux sockets under /run, which is more secure than /tmp
      TMUX_TMPDIR = optional pkgs.stdenv.isLinux ''''${XDG_RUNTIME_DIR:-/run/user/$(id -u)}'';
    };

    modules.shell.aliases.mux = "tmuxinator";
  };
}

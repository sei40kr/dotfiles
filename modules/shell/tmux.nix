{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) binDir;
  cfg = config.modules.shell.tmux;

  tmux-wrapped = pkgs.symlinkJoin {
    name = "tmux-wrapped";

    paths = with pkgs; [ tmux ];
    nativeBuildInputs = with pkgs; [ makeWrapper ];

    postBuild = ''
      wrapProgram $out/bin/tmux --set __ETC_BASHRC_SOURCED "" \
                                --set __ETC_ZSHENV_SOURCED "" \
                                --set __ETC_ZPROFILE_SOURCED  "" \
                                --set __ETC_ZSHRC_SOURCED "" \
                                --set __NIX_SET_ENVIRONMENT_DONE "" \
                                --set __NIX_DARWIN_SET_ENVIRONMENT_DONE ""
    '';
  };

  inherit (config.modules.term.theme) colors;
  tmux_conf = pkgs.runCommandLocal "tmux.conf" { } ''
    substitute ${../../config/tmux/tmux.conf} $out \
      --subst-var-by border_active   ${escapeShellArg "#${colors.border.active}"} \
      --subst-var-by border_inactive ${escapeShellArg "#${colors.border.inactive}"} \
      --subst-var-by selection_bg    ${escapeShellArg "#${colors.selection.bg}"} \
      --subst-var-by selection_fg    ${escapeShellArg "#${colors.selection.fg}"} \
      --subst-var-by tab_active_bg   ${escapeShellArg "#${colors.tab.active.bg}"} \
      --subst-var-by tab_active_fg   ${escapeShellArg "#${colors.tab.active.fg}"} \
      --subst-var-by tab_bg          ${escapeShellArg "#${colors.tab.bg}"} \
      --subst-var-by tab_inactive_bg ${escapeShellArg "#${colors.tab.inactive.bg}"} \
      --subst-var-by tab_inactive_fg ${escapeShellArg "#${colors.tab.inactive.fg}"} \
      --subst-var-by copycat      ${pkgs.tmuxPlugins.copycat.rtp} \
      --subst-var-by cowboy       ${pkgs.my.tmuxPlugins.cowboy.rtp} \
      --subst-var-by ghq          ${pkgs.my.tmuxPlugins.ghq.rtp} \
      --subst-var-by open         ${pkgs.tmuxPlugins.open.rtp} \
      --subst-var-by pain_control ${pkgs.tmuxPlugins.pain-control.rtp} \
      --subst-var-by sessionist   ${pkgs.tmuxPlugins.sessionist.rtp} \
      --subst-var-by urlview      ${pkgs.tmuxPlugins.urlview.rtp} \
      --subst-var-by yank         ${pkgs.tmuxPlugins.yank.rtp}
  '';
in
{
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;

    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [
      tmux-wrapped
      pkgs.tmuxinator
      # VTE terminals (ex. GNOME Terminal) does not support "Ms" capability.
      # See https://github.com/tmux/tmux/wiki/Clipboard#terminal-support---vte-terminals
      (mkIf pkgs.stdenv.isLinux pkgs.wl-clipboard)
    ];

    home.configFile."tmux/tmux.conf".source = tmux_conf;

    environment.variables = {
      # Store the tmux sockets under /run, which is more secure than /tmp
      TMUX_TMPDIR = optional pkgs.stdenv.isLinux ''''${XDG_RUNTIME_DIR:-"/run/user/$(id -u)"}'';
    };

    modules.shell.aliases.mux = "tmuxinator";
  };
}

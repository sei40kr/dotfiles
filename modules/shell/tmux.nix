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
  tmux_conf = pkgs.substituteAll {
    src = ../../config/tmux/tmux.conf;

    # Colors
    border_active = "#${colors.border.active}";
    border_inactive = "#${colors.border.inactive}";
    selection_bg = "#${colors.selection.bg}";
    selection_fg = "#${colors.selection.fg}";
    tab_active_bg = "#${colors.tab.active.bg}";
    tab_active_fg = "#${colors.tab.active.fg}";
    tab_bg = "#${colors.tab.bg}";
    tab_inactive_bg = "#${colors.tab.inactive.bg}";
    tab_inactive_fg = "#${colors.tab.inactive.fg}";

    # Plugins
    copycat = pkgs.tmuxPlugins.copycat.rtp;
    cowboy = pkgs.my.tmuxPlugins.cowboy.rtp;
    ghq = pkgs.my.tmuxPlugins.ghq.rtp;
    open = pkgs.tmuxPlugins.open.rtp;
    pain_control = pkgs.tmuxPlugins.pain-control;
    sessionist = pkgs.tmuxPlugins.sessionist.rtp;
    urlview = pkgs.tmuxPlugins.urlview.rtp;
    yank = pkgs.tmuxPlugins.yank.rtp;
  };
in {
  options.modules.shell.tmux = {
    enable = mkBoolOpt false;
    autoRun = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = [ tmux-wrapped pkgs.tmuxinator ];

    home.configFile."tmux/tmux.conf".source = tmux_conf;

    modules.shell.aliases.mux = "tmuxinator";
  };
}

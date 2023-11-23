{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (pkgs) stdenv;
  termCfg = config.modules.term;
  inherit (termCfg.colorschemes.colors) fg bg ansi cursor link selection paneBorder tabBar;
  cfg = termCfg.wezterm;
in
{
  options.modules.term.wezterm = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ wezterm ];

    home.configFile."wezterm/wezterm.lua".text = ''
      local wezterm = require("wezterm")

      local act = wezterm.action

      local tmux = require("tmux")
      local pain_control = require("pain-control")
      local per_project_workspace = require("per-project-workspace")

      local config = {}

      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.show_update_window = false
      config.automatically_reload_config = false

      config.font = wezterm.font("${termCfg.font.name}")
      config.font_size = ${toString termCfg.font.size}

      config.use_fancy_tab_bar = false

      config.set_environment_variables = {
        ${optionalString stdenv.isDarwin ''
          LANG = "en_US.UTF-8",
        ''}
      }

      config.tab_bar_at_bottom = true

      config.colors = {
        foreground = "#${fg}",
        background = "#${bg}",

        cursor_bg = "#${cursor.bg}",
        cursor_fg = "#${cursor.fg}",

        selection_bg = "#${selection.bg}",
        selection_fg = "#${selection.fg}",

        split = "#${paneBorder.default}",

        ansi = {
          "#${ansi.black}",
          "#${ansi.red}",
          "#${ansi.green}",
          "#${ansi.yellow}",
          "#${ansi.blue}",
          "#${ansi.magenta}",
          "#${ansi.cyan}",
          "#${ansi.white}",
        },
        brights = {
          "#${ansi.brightBlack}",
          "#${ansi.brightRed}",
          "#${ansi.brightGreen}",
          "#${ansi.brightYellow}",
          "#${ansi.brightBlue}",
          "#${ansi.brightMagenta}",
          "#${ansi.brightCyan}",
          "#${ansi.brightWhite}",
        },

        tab_bar = {
          background = "#${tabBar.bg}",

          active_tab = {
            bg_color = "#${tabBar.activeTab.bg}",
            fg_color = "#${tabBar.activeTab.fg}",
            intensity = "Bold",
          },

          inactive_tab = {
            bg_color = "#${tabBar.inactiveTab.bg}",
            fg_color = "#${tabBar.inactiveTab.fg}",
          },
        },
      }

      config.keys = {
        { key = "r", mods = "LEADER|SHIFT", action = act.ReloadConfiguration },
      }

      tmux.apply_to_config(config, { leader = { key = "t", mods = "CTRL" } })

      pain_control.apply_to_config(config, {})

      per_project_workspace.apply_to_config(config, {})
      table.insert(config.keys, {
        key = "g",
        mods = "LEADER",
        action = per_project_workspace.action.ProjectWorkspaceSelect({
          base_dirs = {
            {
              path = "/etc/dotfiles",
              min_depth = 0,
              max_depth = 0,
            },
            {
              path = wezterm.home_dir .. "/ghq",
              min_depth = 3,
              max_depth = 3,
            },
          },
        }),
      })

      return config
    '';
    home.configFile."wezterm/tmux.lua".source = "${configDir}/wezterm/tmux.lua";
    home.configFile."wezterm/pain-control.lua".source = "${configDir}/wezterm/pain-control.lua";
    home.configFile."wezterm/per-project-workspace.lua".source = "${configDir}/wezterm/per-project-workspace.lua";
  };
}

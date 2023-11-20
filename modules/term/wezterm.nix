{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  inherit (pkgs) stdenv;
  termCfg = config.modules.term;
  inherit (termCfg.theme) colors;
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
        foreground = "#${colors.fg}",
        background = "#${colors.bg}",

        cursor_bg = "#${colors.cursor.bg}",
        cursor_fg = "#${colors.cursor.fg}",

        selection_bg = "#${colors.selection.bg}",
        selection_fg = "#${colors.selection.fg}",

        split = "#${colors.border.inactive}",

        ansi = {
          "#${colors.base0}",
          "#${colors.base1}",
          "#${colors.base2}",
          "#${colors.base3}",
          "#${colors.base4}",
          "#${colors.base5}",
          "#${colors.base6}",
          "#${colors.base7}",
        },
        brights = {
          "#${colors.base8}",
          "#${colors.base9}",
          "#${colors.base10}",
          "#${colors.base11}",
          "#${colors.base12}",
          "#${colors.base13}",
          "#${colors.base14}",
          "#${colors.base15}",
        },

        tab_bar = {
          background = "#${colors.tab.bg}",

          active_tab = {
            bg_color = "#${colors.tab.active.bg}",
            fg_color = "#${colors.tab.active.fg}",
            intensity = "Bold",
          },

          inactive_tab = {
            bg_color = "#${colors.tab.inactive.bg}",
            fg_color = "#${colors.tab.inactive.fg}",
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

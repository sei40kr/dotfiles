{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (pkgs) stdenv;
  inherit (stdenv.hostPlatform) system;
  termCfg = config.modules.term;
  inherit (termCfg.colorschemes.colors) fg bg ansi cursor selection paneBorder tabBar;
  cfg = termCfg.wezterm;

  plugins = stdenv.mkDerivation {
    name = "wezterm-plugins";

    phases = [ "installPhase" ];

    buildInputs = [
      inputs.wez-tmux.packages.${system}.default
      inputs.wez-pain-control.packages.${system}.default
      inputs.wez-per-project-workspace.packages.${system}.default
    ];

    installPhase = ''
      mkdir -p $out/share/wezterm
      cp -r ${inputs.wez-tmux.packages.${system}.default} $out/share/wezterm/wez-tmux
      cp -r ${inputs.wez-pain-control.packages.${system}.default} $out/share/wezterm/wez-pain-control
      cp -r ${inputs.wez-per-project-workspace.packages.${system}.default} $out/share/wezterm/wez-per-project-workspace
    '';
  };
in
{
  options.modules.term.wezterm = with types; { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ wezterm ];

    home.configFile."wezterm/wezterm.lua".text = ''
      package.path = package.path .. ";${plugins}/share/wezterm/?.lua;${plugins}/share/wezterm/?/init.lua"

      local wezterm = require("wezterm")

      local act = wezterm.action

      local config = {}

      if wezterm.config_builder then
        config = wezterm.config_builder()
      end

      config.show_update_window = false
      config.automatically_reload_config = false

      config.font = wezterm.font("${termCfg.font.name}")
      config.font_size = ${toString termCfg.font.size}
      config.freetype_load_target = "HorizontalLcd"
      config.warn_about_missing_glyphs = false

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

      config.term = "wezterm"

      config.leader = { key = "t", mods = "CTRL" }

      config.keys = {
        { key = "r", mods = "LEADER|SHIFT", action = act.ReloadConfiguration },
      }

      wezterm.on("window-config-reloaded", function(window, _)
        window:toast_notification("wezterm", "Configuration reloaded!")
      end)

      require("wez-tmux").apply_to_config(config, {})

      require("wez-pain-control").apply_to_config(config, {})

      local per_project_workspace = require("wez-per-project-workspace")
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
  };
}


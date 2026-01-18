{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  inherit (pkgs) stdenv;
  termCfg = config.modules.term;
  inherit (termCfg.colorschemes.colors)
    fg
    bg
    ansi
    cursor
    selection
    paneBorder
    tabBar
    statusLine
    ;
  cfg = termCfg.wezterm;

  plugins = stdenv.mkDerivation {
    name = "wezterm-plugins";

    phases = [ "installPhase" ];

    buildInputs = [
      perSystem.wez-tmux.default
      perSystem.wez-pain-control.default
      perSystem.wez-per-project-workspace.default
      perSystem.wez-status-generator.default
    ];

    installPhase = ''
      mkdir -p $out/share/wezterm
      cp -r ${perSystem.wez-tmux.default} $out/share/wezterm/wez-tmux
      cp -r ${perSystem.wez-pain-control.default} $out/share/wezterm/wez-pain-control
      cp -r ${perSystem.wez-per-project-workspace.default} $out/share/wezterm/wez-per-project-workspace
      cp -r ${perSystem.wez-status-generator.default} $out/share/wezterm/wez-status-generator
    '';
  };
in
{
  options.modules.term.wezterm = {
    enable = mkEnableOption "WezTerm";
  };

  config = mkIf cfg.enable {
    programs.wezterm = {
      enable = true;
      extraConfig = ''
        package.path = package.path .. ";${plugins}/share/wezterm/?.lua;${plugins}/share/wezterm/?/init.lua"

        local wezterm = require("wezterm")

        local act = wezterm.action
        local nerdfonts = wezterm.nerdfonts

        local config = {}

        if wezterm.config_builder then
          config = wezterm.config_builder()
        end

        config.automatically_reload_config = false

        -- NOTE: The text rendering of WezTerm in Nixpkgs is broken with OpenGL
        --  renderer:
        --  https://github.com/wez/wezterm/issues/5990
        config.front_end = "WebGpu"

        config.font = wezterm.font_with_fallback({
          "${termCfg.font.name}",
          "Symbols Nerd Font Mono",
        })
        config.font_size = ${toString termCfg.font.size}
        config.harfbuzz_features = { "calt=0", "clig=0", "liga=0" }
        config.warn_about_missing_glyphs = false

        config.hide_mouse_cursor_when_typing = false

        config.use_fancy_tab_bar = false

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

        config.window_background_opacity = ${toString termCfg.bgOpacity}
        config.macos_window_background_blur = ${toString termCfg.bgBlur}

        config.term = "wezterm"

        config.leader = { key = "t", mods = "CTRL" }

        config.keys = {
          { key = "r", mods = "LEADER|SHIFT", action = act.ReloadConfiguration },
        }

        local status_generator = require("wez-status-generator")
        wezterm.on("update-status", function(window, pane)
          local left_status = status_generator.generate_left_status({
            sections = {
              {
                components = {
                  function()
                    return window:mux_window():get_workspace():gsub(".*/", "")
                  end,
                },
                foreground = "#${statusLine.sections.a.fg}",
                background = "#${statusLine.sections.a.bg}",
              },
              {
                components = {
                  function()
                    local domain_name = pane:get_domain_name()

                    if domain_name ~= "local" then
                      return domain_name
                    end
                  end,
                },
                foreground = "#${statusLine.sections.b.fg}",
                background = "#${statusLine.sections.b.bg}",
              },
            },
            separator = status_generator.separators.SLANT_REVERSE,
            hide_empty_sections = false,
          })
          local right_status = status_generator.generate_right_status({
            sections = {
              {
                components = {
                  function()
                    local ok, battery_infos = pcall(wezterm.battery_info)

                    if not ok or #battery_infos <= 0 or battery_infos[1].state == "Unknown" then
                      return
                    end

                    local battery_info = battery_infos[1]
                    local icon
                    local percentage = battery_info.state_of_charge * 100

                    if battery_info.state == "Charging" then
                      icon = "md_battery_charging_" .. math.floor(percentage / 10 + 0.5) * 10
                    elseif battery_info.state == "Discharging" then
                      icon = "md_battery_" .. math.floor(percentage / 10 + 0.5) * 10
                    elseif battery_info.state == "Empty" then
                      icon = "md_battery_outline"
                    elseif battery_info.state == "Full" then
                      icon = "md_battery"
                    end
                    -- There're no icons for 0% and 100%.
                    icon = icon:gsub("_0", "_outline"):gsub("_100", "")

                    return string.format("%s %.0f%% ", nerdfonts[icon], percentage)
                  end,
                },
                foreground = "#${statusLine.sections.x.fg}",
                background = "#${statusLine.sections.x.bg}",
              },
              {
                components = {
                  function()
                    return nerdfonts.md_clock_outline .. " " .. wezterm.strftime("%-m/%-d %a, %H:%M")
                  end,
                },
                foreground = "#${statusLine.sections.y.fg}",
                background = "#${statusLine.sections.y.bg}",
              },
            },
            separator = status_generator.separators.SLANT_REVERSE,
          })

          window:set_left_status(left_status .. (" "):rep(2))
          window:set_right_status(right_status)
        end)

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

    home.packages = with pkgs; [ nerd-fonts.symbols-only ];
  };
}

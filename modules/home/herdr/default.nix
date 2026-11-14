{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (builtins) fromJSON toJSON;
  inherit (lib)
    concatMapStrings
    escapeShellArg
    genList
    getExe
    listToAttrs
    mkEnableOption
    mkIf
    nameValuePair
    optionalAttrs
    ;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;
  termCfg = config.modules.term;
  cfg = termCfg.herdr;

  inherit (perSystem.llm-agents-nix) herdr-app herdr-core;

  inherit (termCfg.colorschemes) colors;

  # tmux-style bindings behind prefix, followed by a key.
  prefixed = lib.mapAttrs' (key: nameValuePair "ctrl-t ${key}") (
    {
      n = "tab.next";
      p = "tab.prev";
      "," = "tab.rename";
      "&" = "tab.close";
      x = "pane.close";
      z = "pane.zoom";
      C = "space.new";
      X = "space.close";
      "$" = "space.rename";
      "(" = "space.prev";
      ")" = "space.next";
      s = "mod.open:space-picker";
      w = "mod.open:picker";
      "ctrl-s" = "mod.open:agent-picker";
      g = "mod.open:repository-picker";
      "]" = "terminal.paste";
      "ctrl-b" = "layout.toggle:sidebar";
      ":" = "app.palette";
      "?" = "app.help";
    }
    // listToAttrs (genList (i: nameValuePair (toString (i + 1)) "tab.select:${toString (i + 1)}") 9)
    // {
      # tmux-sensible
      # Nix strings have no \u escapes; JSON has.
      "ctrl-t" = "send:${fromJSON ''"\u0014"''}";
      "ctrl-p" = "tab.prev";
      "ctrl-n" = "tab.next";

      # tmux-pain-control (no commands for < > swap and H/J/K/L resize)
      h = "pane.go:left";
      "ctrl-h" = "pane.go:left";
      j = "pane.go:down";
      "ctrl-j" = "pane.go:down";
      k = "pane.go:up";
      "ctrl-k" = "pane.go:up";
      l = "pane.go:right";
      "ctrl-l" = "pane.go:right";
      "|" = "pane.split:right";
      "\\" = "pane.split:right";
      "%" = "pane.split:right";
      "-" = "pane.split:down";
      "_" = "pane.split:down";
      "\"" = "pane.split:down";
      c = "tab.new";
    }
  );
in
{
  options.modules.term.herdr = {
    enable = mkEnableOption "Herdr";
  };

  # TODO: Use home-manager's programs.herdr once it covers herdr-app.
  config = mkIf cfg.enable {
    # herdr-core for `hc` in shells; herdr-app finds its own.
    home.packages = [
      herdr-app
      herdr-core
    ];

    # Each file is placed on its own: the app writes state next to them, so
    # ~/.config/herdr-app and its subfolders must stay writable.
    xdg.configFile."herdr-app/settings.json".text = toJSON {
      font = termCfg.font.name;
      font_size = termCfg.font.size;
      theme = "tokyo-night";
      sounds.enabled = true;
      option_as_alt = "on";
    };
    xdg.configFile."herdr-app/themes/tokyo-night.json".text = toJSON {
      background = "#${colors.bg}";
      foreground = "#${colors.fg}";
      selection = "#${colors.selection.bg}";
      cursor = "#${colors.cursor.bg}";
      ansi = map (c: "#${c}") (
        with colors.ansi;
        [
          black
          red
          green
          yellow
          blue
          magenta
          cyan
          white
          brightBlack
          brightRed
          brightGreen
          brightYellow
          brightBlue
          brightMagenta
          brightCyan
          brightWhite
        ]
      );
    };
    xdg.configFile."herdr-app/keys.json".text = toJSON {
      global = {
        "alt-h" = "pane.go:left";
        "alt-j" = "pane.go:down";
        "alt-k" = "pane.go:up";
        "alt-l" = "pane.go:right";
        f2 = "tab.rename";
        "shift-f2" = "space.rename";
      }
      // optionalAttrs isDarwin {
        "cmd-q" = "app.quit";
        "cmd-h" = "app.hide";
      }
      // prefixed;
      # keys.json replaces Herdr's defaults whole; these are its copy and paste.
      terminal =
        if isDarwin then
          {
            "cmd-c" = "terminal.copy";
            "cmd-v" = "terminal.paste";
          }
        else
          {
            "ctrl-shift-c" = "terminal.copy";
            "ctrl-shift-v" = "terminal.paste";
          };
    };
    # Written by herdrAppSettings below.
    xdg.configFile."herdr-app/settings.json".enable = false;
    xdg.configFile."herdr-app/themes/tokyo-night.json".enable = false;
    xdg.configFile."herdr-app/keys.json".enable = false;
    xdg.configFile."herdr-app/mods/repository-picker/mod.json".text = toJSON {
      place = "none";
      title = "Repositories";
      overlay = {
        kind = "popup";
        at = "top";
        offset = 40;
        width = 640;
        height = 0.8;
      };
    };
    xdg.configFile."herdr-app/mods/repository-picker/main.js".source =
      pkgs.replaceVars ./repository-picker.js
        {
          ghq = getExe pkgs.ghq;
        };
    xdg.configFile."herdr-app/mods/space-picker/mod.json".text = toJSON {
      place = "none";
      title = "Spaces";
      overlay = {
        kind = "popup";
        at = "top";
        offset = 40;
        width = 640;
        height = 0.8;
      };
    };
    xdg.configFile."herdr-app/mods/space-picker/main.js".source = ./space-picker.js;
    xdg.configFile."herdr-app/mods/agent-picker/mod.json".text = toJSON {
      place = "none";
      title = "Agents";
      overlay = {
        kind = "popup";
        at = "top";
        offset = 40;
        width = 640;
        height = 0.8;
      };
    };
    xdg.configFile."herdr-app/mods/agent-picker/main.js".source = ./agent-picker.js;

    # Copied, not linked: herdr-app reloads on mtime, and store files never change it.
    home.activation.herdrAppSettings = lib.hm.dag.entryAfter [ "linkGeneration" ] (
      concatMapStrings
        (
          name:
          let
            file = config.xdg.configFile."herdr-app/${name}";
            dest = escapeShellArg "${config.home.homeDirectory}/${file.target}";
          in
          ''
            run install -Dm644 ${file.source} ${dest}.tmp
            run mv -f ${dest}.tmp ${dest}
          ''
        )
        [
          "settings.json"
          "themes/tokyo-night.json"
          "keys.json"
        ]
    );

    modules.ai.skillPaths = [ "${herdr-core}/share/skills/herdr-core" ];
  };
}

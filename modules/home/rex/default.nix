{
  config,
  inputs,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (builtins) toJSON;
  inherit (lib)
    getExe
    mkEnableOption
    mkIf
    mkOption
    optionals
    ;
  inherit (lib.types) attrs attrsOf;
  inherit (pkgs.stdenv.hostPlatform) isDarwin;

  termCfg = config.modules.term;
  cfg = termCfg.rex;
  inherit (termCfg.colorschemes) active;

  theme = cfg.themes.${active};
in
{
  imports = [ inputs.rex.homeModules.default ];

  options.modules.term.rex = {
    enable = mkEnableOption "Rex";

    # Rex.app bundles no themes for the colorschemes, so each one defines its own.
    themes = mkOption {
      type = attrsOf attrs;
      default = { };
      internal = true;
      description = "Rex custom themes by colorscheme name, as Rex.app stores one under `Workspace.customThemes`.";
    };
  };

  config = mkIf cfg.enable {
    programs.rex = {
      enable = true;
      package = perSystem.self.rex;
      config = pkgs.replaceVars ./init.lua {
        bfs = getExe pkgs.bfs;
      };
      # The guard lives in ~/.ssh/config, which only home-manager's ssh module
      # writes; without it Rex falls back to a TERM that remotes know.
      sshGuard = config.programs.ssh.enable;
    };

    home.packages = optionals isDarwin [ perSystem.self.rex-app ];

    targets.darwin.defaults."com.superlogical.rex" = mkIf isDarwin {
      # Sparkle can't replace an app in the read-only store; run packages/rex-app/update.sh instead.
      SUEnableAutomaticChecks = false;
      "Workspace.terminalFontName" = termCfg.font.name;
      "Workspace.terminalFontPointSize" = termCfg.font.size;
      "Workspace.terminalFontSmoothing" = true;
      "Workspace.appearance" = "dark";
      "Workspace.darkThemeID" = theme.id;
      "Workspace.animationStyle" = "off";
      "Workspace.paneDensity" = "compact";
      "Workspace.tabBarEdge" = "bottom";
    };

    # Rex.app reads this key as data, which targets.darwin.defaults can't write.
    home.activation.rexCustomThemes = mkIf isDarwin (
      lib.hm.dag.entryAfter [ "setDarwinDefaults" ] ''
        run /usr/bin/defaults write com.superlogical.rex Workspace.customThemes -data \
          "$(/usr/bin/xxd -p ${pkgs.writeText "rex-custom-themes.json" (toJSON [ theme ])} | tr -d '\n')"
      ''
    );

    modules.ai.skillPaths = [ "${config.programs.rex.package}/share/skills/rex" ];

    # What `rex skill setup` adds to Claude Code besides the skill: a hook
    # telling each session where it runs in Rex.
    programs.claude-code.settings.hooks.SessionStart = [
      {
        hooks = [
          {
            type = "command";
            # Verbatim, so that `rex skill status` recognizes it.
            command = "/bin/sh -c 'rex __internal session-start --agent claude --feature-session-naming=false 2>/dev/null || true'";
            timeout = 5;
          }
        ];
      }
    ];
  };
}

{
  config,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.workmux;

  inherit (perSystem.llm-agents-nix) workmux;
  workmuxBin = "${workmux}/bin/workmux";
in
{
  options.modules.dev.tools.workmux = {
    enable = mkEnableOption "workmux";
  };

  config = mkIf cfg.enable {
    home.packages = [ workmux ];

    home.shellAliases = {
      wm = "workmux";
    };

    xdg.configFile."workmux/config.yaml" = {
      force = true;
      text = ''
        nerdfont: true
        mode: session

        panes:
          - command: <agent>
            focus: true

        files:
          copy:
            - .claude/settings.local.json
      '';
    };

    modules.ai.skillPaths = [
      "${workmux}/share/workmux/skills"
    ];

    modules.ai.permissions.allowedCommandPrefixes = [
      "workmux list"
      "workmux ls"
      "workmux open"
      "workmux dashboard"
      "workmux config"
    ];

    # Status tracking hooks (equivalent to `workmux setup --hooks`)
    programs.claude-code.settings.hooks = {
      UserPromptSubmit = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status working";
            }
          ];
        }
      ];
      Notification = [
        {
          matcher = "permission_prompt|elicitation_dialog";
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status waiting";
            }
          ];
        }
      ];
      PostToolUse = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status working";
            }
          ];
        }
      ];
      Stop = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status done";
            }
          ];
        }
      ];
    };
    programs.gemini-cli.settings.hooks = {
      BeforeAgent = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status working";
            }
          ];
        }
      ];
      Notification = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status waiting";
            }
          ];
        }
      ];
      AfterTool = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status working";
            }
          ];
        }
      ];
      AfterAgent = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status done";
            }
          ];
        }
      ];
      SessionEnd = [
        {
          hooks = [
            {
              type = "command";
              command = "${workmuxBin} set-window-status done";
            }
          ];
        }
      ];
    };
  };
}

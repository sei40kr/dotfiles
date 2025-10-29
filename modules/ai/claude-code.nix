{
  config,
  inputs',
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    ;
  cfg = config.modules.ai.claude-code;
in
{
  options.modules.ai.claude-code = {
    enable = mkEnableOption "Claude Code";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = [ inputs'.llm-agents-nix.packages.claude-code ];

    home.file.".claude/settings.json".text = builtins.toJSON {
      env = {
        CLAUDE_CODE_IDE_SKIP_AUTO_INSTALL = 1;
        DISABLE_AUTOUPDATER = 1;
      };
      hooks = {
        Notification = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Action required or input idle.'";
              }
            ];
          }
        ];
        Stop = [
          {
            hooks = [
              {
                type = "command";
                command = "${pkgs.libnotify}/bin/notify-send 'Claude Code' 'Response complete!'";
              }
            ];
          }
        ];
      };
    };
  };
}

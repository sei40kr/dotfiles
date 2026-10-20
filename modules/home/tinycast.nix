{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.apps.tinycast;
  app = "${perSystem.self.tinycast}/Applications/Tinycast Beta.app";
  # Carbon modifier cmd (256).
  snapHotkey =
    keyCode:
    builtins.toJSON {
      combo._0 = {
        carbonModifiers = 256;
        carbonKeyCode = keyCode;
      };
    };
in
{
  options.modules.desktop.apps.tinycast = {
    enable = mkEnableOption "Tinycast";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.hostPlatform.isDarwin;
        message = "Tinycast is only available on macOS.";
      }
    ];

    home.packages = [ perSystem.self.tinycast ];

    # Tinycast is an LSUIElement agent, so it has to stay resident for its
    # global hotkey to respond.
    launchd.agents.tinycast = {
      enable = true;
      config = {
        ProgramArguments = [ "${app}/Contents/MacOS/Tinycast Beta" ];
        RunAtLoad = true;
        KeepAlive = true;
      };
    };

    # Windows-style snap on cmd+arrows.
    targets.darwin.defaults."com.tinycast.app.beta" = {
      windowManagementEnabled = true;
      "hotkey.windowCommand.left-half" = snapHotkey 123;
      "hotkey.windowCommand.right-half" = snapHotkey 124;
      "hotkey.windowCommand.restore" = snapHotkey 125;
      "hotkey.windowCommand.maximize" = snapHotkey 126;
    };

    # Tinycast reads hotkeys only at launch, so restart it to pick up the new defaults.
    home.activation.restartTinycast =
      lib.hm.dag.entryAfter [ "setDarwinDefaults" "setupLaunchAgents" ]
        ''
          run /bin/launchctl kickstart -k "gui/$(/usr/bin/id -u)/org.nix-community.home.tinycast" || true
        '';
  };
}

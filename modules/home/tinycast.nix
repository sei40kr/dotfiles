{
  config,
  lib,
  perSystem,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.modules.desktop.apps.tinycast;
  app = "${perSystem.self.tinycast}/Applications/Tinycast.app";
  # Carbon modifiers: cmd (256), shift (512), option (2048).
  hotkey =
    carbonModifiers: carbonKeyCode:
    builtins.toJSON {
      combo._0 = { inherit carbonModifiers carbonKeyCode; };
    };
  cmdHotkey = hotkey 256;
  optHotkey = hotkey 2048;
  optShiftHotkey = hotkey (2048 + 512);
in
{
  options.modules.desktop.apps.tinycast = {
    enable = mkEnableOption "Tinycast";

    extensions = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = "Prebuilt Raycast extensions to install, such as `perSystem.self.raycastExtensions.pomodoro`.";
    };
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
        ProgramArguments = [ "${app}/Contents/MacOS/Tinycast" ];
        RunAtLoad = true;
        KeepAlive = true;
      };
    };

    targets.darwin.defaults."com.tinycast.app" = {
      "hotkey.togglePalette" = cmdHotkey 49;

      # Windows-style snap on cmd+arrows.
      windowManagementEnabled = true;
      "hotkey.windowCommand.left-half" = cmdHotkey 123;
      "hotkey.windowCommand.right-half" = cmdHotkey 124;
      "hotkey.windowCommand.restore" = cmdHotkey 125;
      "hotkey.windowCommand.maximize" = cmdHotkey 126;

      # Carbon hotkeys can't take over cmd+tab from the Dock, so use option+tab (Windows' alt+tab).
      navigationEnabled = true;
      "hotkey.command:switch-windows" = optHotkey 48;
      "hotkey.command:switch-windows-backwards" = optShiftHotkey 48;
    }
    // lib.optionalAttrs (cfg.extensions != [ ]) { extensionsEnabled = true; };

    # Tinycast loads every extension directory here at launch.
    home.file = lib.listToAttrs (
      map (
        ext:
        lib.nameValuePair "Library/Application Support/com.tinycast.app/extensions/${ext.pname}" {
          source = ext;
        }
      ) cfg.extensions
    );

    # Free cmd+space for Tinycast: input sources on ctrl(+shift)+space, Spotlight off.
    # defaults import would replace the whole AppleSymbolicHotKeys dict, so patch each entry.
    home.activation.setSpaceHotkeys =
      let
        # IDs: 60/61 previous/next input source, 64 Spotlight, 65 Finder search window.
        # Modifier flags: shift (131072), ctrl (262144), option (524288), cmd (1048576).
        setSymbolicHotkey = id: enabled: modifiers: ''
          run /usr/bin/defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add ${toString id} \
            '<dict><key>enabled</key><${lib.boolToString enabled}/><key>value</key><dict><key>parameters</key><array><integer>65535</integer><integer>49</integer><integer>${toString modifiers}</integer></array><key>type</key><string>standard</string></dict></dict>'
        '';
      in
      lib.hm.dag.entryAfter [ "writeBoundary" ] ''
        ${setSymbolicHotkey 60 true (262144 + 131072)}
        ${setSymbolicHotkey 61 true 262144}
        ${setSymbolicHotkey 64 false 1048576}
        ${setSymbolicHotkey 65 false (1048576 + 524288)}
        run /System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u
      '';

    # Tinycast reads hotkeys only at launch, so restart it to pick up the new defaults.
    home.activation.restartTinycast =
      lib.hm.dag.entryAfter [ "setDarwinDefaults" "setupLaunchAgents" ]
        ''
          run /bin/launchctl kickstart -k "gui/$(/usr/bin/id -u)/org.nix-community.home.tinycast" || true
        '';
  };
}

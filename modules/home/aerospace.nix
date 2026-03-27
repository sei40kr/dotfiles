{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.desktop.aerospace;
in
{
  options.modules.desktop.aerospace = {
    enable = mkEnableOption "AeroSpace";
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = pkgs.stdenv.isDarwin;
        message = "AeroSpace is only supported on macOS";
      }
    ];

    programs.aerospace = {
      enable = true;
      launchd.enable = true;
      userSettings = {
        after-startup-command = [ ];

        # Normalizations
        enable-normalization-flatten-containers = true;
        enable-normalization-opposite-orientation-for-nested-containers = true;

        # Layout
        accordion-padding = 30;
        default-root-container-layout = "tiles";
        default-root-container-orientation = "auto";

        gaps = {
          inner.horizontal = 16;
          inner.vertical = 16;
          outer.left = 32;
          outer.bottom = 32;
          outer.top = 32;
          outer.right = 32;
        };

        on-window-detected = [
          # floating
          {
            "if".app-id = "com.apple.systempreferences";
            run = [ "layout floating" ];
          }
          # web
          {
            "if".app-id = "com.microsoft.edgemac";
            run = [ "move-node-to-workspace web" ];
          }
          # dev
          {
            "if".app-id = "com.mitchellh.ghostty";
            run = [ "move-node-to-workspace dev" ];
          }
          # media
          {
            "if".app-id = "com.apple.finder";
            run = [ "move-node-to-workspace media" ];
          }
          {
            "if".app-id = "com.microsoft.Word";
            run = [ "move-node-to-workspace media" ];
          }
          {
            "if".app-id = "com.microsoft.Excel";
            run = [ "move-node-to-workspace media" ];
          }
          {
            "if".app-id = "com.microsoft.Powerpoint";
            run = [ "move-node-to-workspace media" ];
          }
          # chat
          {
            "if".app-id = "com.microsoft.Outlook";
            run = [ "move-node-to-workspace chat" ];
          }
          {
            "if".app-id = "com.tinyspeck.slackmacgap";
            run = [ "move-node-to-workspace chat" ];
          }
          {
            "if".app-id = "com.microsoft.teams2";
            run = [ "move-node-to-workspace chat" ];
          }
        ];

        mode.main.binding = {
          # Focus (Regolith: Mod+h/j/k/l)
          "alt-h" = "focus left";
          "alt-j" = "focus down";
          "alt-k" = "focus up";
          "alt-l" = "focus right";

          # Move (Regolith: Mod+Shift+h/j/k/l)
          "alt-shift-h" = "move left";
          "alt-shift-j" = "move down";
          "alt-shift-k" = "move up";
          "alt-shift-l" = "move right";

          # Resize (Regolith: Mod+r)
          "alt-r" = "mode resize";

          # Layout (Regolith: Mod+t, Mod+Backspace, Mod+f, Mod+Shift+f)
          "alt-t" = "layout tiles horizontal vertical";
          "alt-backspace" = "layout accordion horizontal vertical";
          "alt-f" = "macos-native-fullscreen";
          "alt-shift-f" = "layout floating tiling";

          # Workspace (Regolith: Mod+0-9)
          "alt-1" = "workspace web";
          "alt-2" = "workspace dev";
          "alt-3" = "workspace media";
          "alt-4" = "workspace chat";
          "alt-5" = "workspace 5";
          "alt-6" = "workspace 6";
          "alt-7" = "workspace 7";
          "alt-8" = "workspace 8";
          "alt-9" = "workspace 9";

          # Move to Workspace (Regolith: Mod+Shift+0-9)
          "alt-shift-1" = "move-node-to-workspace web";
          "alt-shift-2" = "move-node-to-workspace dev";
          "alt-shift-3" = "move-node-to-workspace media";
          "alt-shift-4" = "move-node-to-workspace chat";
          "alt-shift-5" = "move-node-to-workspace 5";
          "alt-shift-6" = "move-node-to-workspace 6";
          "alt-shift-7" = "move-node-to-workspace 7";
          "alt-shift-8" = "move-node-to-workspace 8";
          "alt-shift-9" = "move-node-to-workspace 9";

          # Session (Regolith: Mod+Shift+q, Mod+Shift+c, Mod+Shift+e)
          "alt-shift-q" = "close";
          "alt-shift-c" = "reload-config";
        };

        mode.resize.binding = {
          "h" = "resize width -50";
          "j" = "resize height +50";
          "k" = "resize height -50";
          "l" = "resize width +50";
          "minus" = "resize smart -50";
          "equal" = "resize smart +50";
          "enter" = "mode main";
          "esc" = "mode main";
        };
      };
    };

    services.jankyborders = {
      enable = true;
      settings = {
        style = "round";
        width = 6.0;
        hidpi = "on";
        active_color = "0xff007aff";
        inactive_color = "0x00000000";
      };
    };
  };
}

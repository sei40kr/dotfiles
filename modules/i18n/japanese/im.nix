{
  config,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  cfg = config.modules.i18n.japanese;

  fcitx5Package = config.i18n.inputMethod.package;
in
{
  config = mkIf (cfg.enable && config.modules.desktop.de.enable) {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        # NOTE: The build of fcitx5-mozc is broken in Nixpkgs as of 24.11
        #  https://github.com/NixOS/nixpkgs/issues/355852
        addons = with pkgs; [ unstable.fcitx5-mozc ];
        settings = {
          globalOptions = {
            Hotkey = {
              # Enumerate when press trigger key repeatedly
              EnumerateWithTriggerKeys = "True";
              # Skip first input method while enumerating
              EnumerateSkipFirst = "False";
            };
            "Hotkey/EnumerateForwardKeys" = {
              "0" = "Control+space";
            };
            "Hotkey/EnumerateBackwardKeys" = {
              "0" = "Control+Shift+space";
            };
            "Hotkey/PrevPage" = {
              "0" = "Up";
            };
            "Hotkey/NextPage" = {
              "0" = "Down";
            };
            "Hotkey/PrevCandidate" = {
              "0" = "Shift+Tab";
            };
            "Hotkey/NextCandidate" = {
              "0" = "Tab";
            };
            Behavior = {
              # Active By Default
              ActiveByDefault = "False";
              # Share Input State
              ShareInputState = "No";
              # Show preedit in application
              PreeditEnabledByDefault = "True";
              # Show Input Method Information when switch input method
              ShowInputMethodInformation = "True";
              # Show Input Method Information when changing focus
              showInputMethodInformationWhenFocusIn = "False";
              # Show compact input method information
              CompactInputMethodInformation = "True";
              # Show first input method information
              ShowFirstInputMethodInformation = "True";
              # Default page size
              DefaultPageSize = "5";
              # Override Xkb Option
              OverrideXkbOption = "False";
              # Preload input method to be used by default
              PreloadInputMethod = "True";
            };
          };
          inputMethod = {
            "Groups/0" = {
              # Group name
              Name = "Default";
              # Layout
              "Default Layout" = "us";
              # Default Input Method
              DefaultIM = "mozc";
            };
            "Groups/0/Items/0" = {
              # Name
              Name = "keyboard-us";
              # Layout
              Layout = "";
            };
            "Groups/0/Items/1" = {
              # Name
              Name = "mozc";
              # Layout
              Layout = "";
            };
            "GroupOrder" = {
              "0" = "Default";
            };
          };
        };
        ignoreUserConfig = true;
      };
    };

    # This still works because Fcitx5 can simulate IBus protocol
    environment.variables.GLFW_IM_MODULE = "ibus";

    # Set the same environment variables as environment.variables to
    # environment.sessionVariables or we can't export them to apps in some
    # cases. See NixOS/nixpkgs#129442.
    environment.sessionVariables = {
      GTK_IM_MODULE = "fcitx";
      QT_IM_MODULE = "fcitx";
      XMODIFIERS = "@im=fcitx";
    };

    systemd.user.services.fcitx5-daemon = {
      description = "Fcitx5 input method editor";
      documentation = [ "https://fcitx-im.org" ];
      aliases = [ "input-method.service" ];
      serviceConfig = {
        ExecStart = "${fcitx5Package}/bin/fcitx5";
      };
    };
  };
}

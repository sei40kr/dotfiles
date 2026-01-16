{
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib) mkIf;
in
{
  config = mkIf (osConfig.modules.i18n.japanese.enable && osConfig.modules.desktop.de.enable) {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
        addons = with pkgs; [ fcitx5-mozc ];
        settings = {
          globalOptions = {
            Hotkey = {
              EnumerateWithTriggerKeys = "True";
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
              ActiveByDefault = "False";
              ShareInputState = "No";
              PreeditEnabledByDefault = "True";
              ShowInputMethodInformation = "True";
              showInputMethodInformationWhenFocusIn = "False";
              CompactInputMethodInformation = "True";
              ShowFirstInputMethodInformation = "True";
              DefaultPageSize = "5";
              OverrideXkbOption = "False";
              PreloadInputMethod = "True";
            };
          };
          inputMethod = {
            "Groups/0" = {
              Name = "Default";
              "Default Layout" = "us";
              DefaultIM = "mozc";
            };
            "Groups/0/Items/0" = {
              Name = "keyboard-us";
              Layout = "";
            };
            "Groups/0/Items/1" = {
              Name = "mozc";
              Layout = "";
            };
            "GroupOrder" = {
              "0" = "Default";
            };
          };
        };
      };
    };
  };
}

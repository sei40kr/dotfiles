{
  lib,
  config,
  pkgs,
  inputs,
  osConfig ? null,
  ...
}:

let
  inherit (lib) attrByPath mkEnableOption mkIf;

  cfg = config.modules.i18n.hazkey;
  hazkeyPkgs = inputs.nix-hazkey.packages.${pkgs.stdenv.hostPlatform.system};
in
{
  imports = [ inputs.nix-hazkey.homeModules.hazkey ];

  options.modules.i18n.hazkey = {
    enable = mkEnableOption "Hazkey (azooKey for Fcitx5)";
  };

  config = mkIf cfg.enable {
    i18n.inputMethod = {
      enable = true;
      type = "fcitx5";
      fcitx5 = {
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
              DefaultIM = "hazkey";
            };
            "Groups/0/Items/0" = {
              Name = "keyboard-us";
              Layout = "";
            };
            "Groups/0/Items/1" = {
              Name = "hazkey";
              Layout = "";
            };
            "GroupOrder" = {
              "0" = "Default";
            };
          };
        };
      };
    };

    services.hazkey = {
      enable = true;
      server.package = mkIf (attrByPath [ "hardware" "graphics" "enable" ] false osConfig) (
        hazkeyPkgs.hazkey-server.override { enableVulkan = true; }
      );
    };
  };
}

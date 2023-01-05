{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.rectangle;
in
{
  options.modules.desktop.apps.rectangle = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ my.rectangle ];

    launchd.user.agents.rectangle = {
      serviceConfig = {
        Label = "com.knollsoft.Rectangle";
        Program = "${pkgs.my.rectangle}/Applications/Rectangle.app/Contents/MacOS/Rectangle";
        KeepAlive = { SuccessfulExit = false; };
        RunAtLoad = true;
      };
    };

    system.defaults.CustomUserPreferences."com.knollsoft.Rectangle" = {
      SUHasLaunchedBefore = false;
      # Don't check for updates automatically
      SUEnableAutomaticChecks = false;
      # Don't launch on login
      launchOnLogin = false;
      # Default Shortcuts: Rectangle
      alternateDefaultShortcuts = true;
      # FIXME: nix-darwin does not support attrset -> dict conversion
      /*
        # Left Half: Cmd-Left
        leftHalf = {
        keyCode = 123;
        modifierFlags = 1048576;
        };
        # Right Half: Cmd-Right
        rightHalf = {
        keyCode = 124;
        modifierFlags = 1048576;
        };
        # Maximize: Cmd-Up
        maximize = {
        keyCode = 126;
        modifierFlags = 1048576;
        };
        topHalf = { };
        bottomHalf = { };
        topLeft = { };
        topRight = { };
        bottomLeft = { };
        bottomRight = { };
        smaller = { };
        larger = { };
        center = { };
        restore = { };
        maximizeHeight = { };
        nextDisplay = { };
        previousDisplay = { };
        firstThird = { };
        centerThird = { };
        lastThird = { };
        firstTwoThirds = { };
        lastTwoThirds = { };
      */
    };
  };
}

{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.material-shell;
in {
  options.modules.desktop.material-shell.enable = mkBoolOpt false;

  config = mkIf cfg.enable {
    assertions = [{
      assertion = config.modules.desktop.gnome.enable;
      message =
        "The material-shell module requires 'modules.desktop.gnome.enable = true'.";
    }];

    user.packages = with pkgs; [ my.gnomeExtensions.material-shell ];

    modules.desktop = {
      dconf = {
        enable = true;
        settings = {
          "org/gnome/desktop/wm/keybindings" = {
            activate-window-menu = [ ];
            begin-move = [ ];
            begin-resize = [ ];
            close = [ ];
            cycle-group = [ ];
            cycle-group-backward = [ ];
            cycle-panels = [ ];
            cycle-panels-backward = [ ];
            cycle-windows = [ ];
            cycle-windows-backward = [ ];
            maximize = [ ];
            minimize = [ ];
            move-to-monitor-down = [ ];
            move-to-monitor-left = [ ];
            move-to-monitor-right = [ ];
            move-to-monitor-up = [ ];
            move-to-workspace-1 = [ ];
            move-to-workspace-last = [ ];
            move-to-workspace-left = [ ];
            move-to-workspace-right = [ ];
            panel-main-menu = [ ];
            panel-run-dialog = [ ];
            switch-applications = [ ];
            switch-applications-backward = [ ];
            switch-group = [ ];
            switch-group-backward = [ ];
            switch-input-source = [ ];
            switch-input-source-backward = [ ];
            switch-panels = [ ];
            switch-panels-backward = [ ];
            switch-to-workspace-1 = [ ];
            switch-to-workspace-last = [ ];
            switch-to-workspace-left = [ ];
            switch-to-workspace-right = [ ];
            switch-windows = [ "<Super>Tab" ];
            switch-windows-backward = [ "<Shift><Super>Tab" ];
            toggle-fullscreen = [ "<Super>f" ];
            toggle-maximized = [ ];
            unmaximize = [ ];
          };
          "org/gnome/shell/keybindings" = {
            focus-active-notification = [ ];
            open-application-menu = [ ];
            switch-application-1 = [ ];
            switch-application-2 = [ ];
            switch-application-3 = [ ];
            switch-application-4 = [ ];
            switch-application-5 = [ ];
            switch-application-6 = [ ];
            switch-application-7 = [ ];
            switch-application-8 = [ ];
            switch-application-9 = [ ];
            toggle-application-view = [ ];
            toggle-message-tray = [ ];
            toggle-overview = [ ];
          };
        };
      };
      gnome.enabledExtensions = [ "material-shell@papyelgringo" ];
    };
  };
}

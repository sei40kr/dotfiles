{ config, lib, pkgs, ... }:

with lib;
with lib.my;
let
  inherit (config.dotfiles) configDir;
  cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = with types; {
    enable = mkBoolOpt false;

    wallpaper = mkOpt (nullOr path) null;
  };

  config = mkIf cfg.enable {
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraSessionCommands = ''
        # SDL
        export SDL_VIDEODRIVER=wayland
        # QT
        export QT_QPA_PLATFORM=wayland-egl
        export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
        # Fix for some Java AWT applications
        export _JAVA_AWT_WM_NONREPARENTING=1
      '';
      extraPackages = with pkgs; [ pulseaudio wl-clipboard ];
    };

    systemd.user = {
      targets.sway-session = {
        description = "Sway compositor session";
        documentation = [ "man:systemd.special(7)" ];
        bindsTo = [ "graphical-session.target" ];
        wants = [ "graphical-session-pre.target" ];
        after = [ "graphical-session-pre.target" ];
      };
      services.swayidle = {
        description = "Idle Manager for Wayland";
        documentation = [ "man:swayidle(1)" ];
        partOf = [ "graphical-session.target" ];
        path = [ pkgs.bash ];
        serviceConfig = {
          # This will lock your screen after 300 seconds of inactivity, then turn off
          # your displays after another 300 seconds, and turn your screens back on when
          # resumed. It will also lock your screen before your computer goes to sleep.
          ExecStart = let lock = "${pkgs.swaylock-effects}/bin/swaylock -f";
          in ''
            ${pkgs.swayidle}/bin/swayidle -w \
              timeout 300 ${escapeShellArg lock} \
              timeout 600 '${pkgs.sway}/bin/swaymsg "output * dpms off"' \
              resume '${pkgs.sway}/bin/swaymsg "output * dpms on"' \
              before-sleep ${escapeShellArg lock}
          '';
        };
        wantedBy = [ "sway-session.target" ];
        restartIfChanged = true;
      };
    };

    environment.etc = {
      # Read `man 5 sway` for a complete reference.
      "sway/config".text = ''
        ### Variables
        #
        # Logo key. Use Mod1 for Alt.
        set $mod Mod4
        # Home row direction keys, like vim
        set $left h
        set $down j
        set $up k
        set $right l

        ### Output configuration

        ${optionalString (cfg.wallpaper != null) ''
          output * bg ${cfg.wallpaper} fill
        ''}


        ### Input configuration
        #
        # Example configuration:
        #
        #   input "2:14:SynPS/2_Synaptics_TouchPad" {
        #       dwt enabled
        #       tap enabled
        #       natural_scroll enabled
        #       middle_emulation enabled
        #   }
        #
        # You can get the names of your inputs by running: swaymsg -t get_inputs
        # Read `man 5 sway-input` for more information about this section.

        ### Key bindings
        #
        # Launch:
        #
          # Application
          bindsym $mod+space exec nwg-drawer -nofs -ovl

          # Browser
          bindsym --no-repeat $mod+Shift+Return exec $DOTFILES_BIN/sway/sensible-browser

          # TODO Command

          # File Browser
          bindsym --no-repeat $mod+Shift+n exec rofi -show filebrowser -show-icons

          # TODO File Search

          # TODO Notification Viewer

          # Terminal
          bindsym --no-repeat $mod+Return exec $DOTFILES_BIN/sway/sensible-terminal

        #
        # Modify:
        #
          # TODO Bluetooth Settings

          # Containing Workspace
          bindsym $mod+Control+Shift+Left move window to workspace prev
          bindsym $mod+Control+Shift+Right move window to workspace next

          # TODO Display Settings

          # Move Window to Workspace 1-9
          bindsym --no-repeat $mod+Shift+1 move window to workspace number 1
          bindsym --no-repeat $mod+Shift+2 move window to workspace number 2
          bindsym --no-repeat $mod+Shift+3 move window to workspace number 3
          bindsym --no-repeat $mod+Shift+4 move window to workspace number 4
          bindsym --no-repeat $mod+Shift+5 move window to workspace number 5
          bindsym --no-repeat $mod+Shift+6 move window to workspace number 6
          bindsym --no-repeat $mod+Shift+7 move window to workspace number 7
          bindsym --no-repeat $mod+Shift+8 move window to workspace number 8
          bindsym --no-repeat $mod+Shift+9 move window to workspace number 9
          # Move Window to Workspace 10
          bindsym --no-repeat $mod+Shift+0 move window to workspace number 10

          # Move to Scratchpad
          bindsym --no-repeat $mod+Control+m move window to scratchpad

          # Next Window Orientation
          bindsym --no-repeat $mod+BackSpace layout toggle split

          # TODO Settings

          # Tile/Float Focus Toggle
          bindsym --no-repeat $mod+Shift+t focus mode_toggle

          # Toggle Bar
          bindsym --no-repeat $mod+i exec ${pkgs.procps}/bin/pkill --signal SIGUSR1 waybar

          # TODO Wi-Fi Settings

          # Window Floating Toggle
          bindsym --no-repeat $mod+Shift+f floating toggle

          # Window Fullscreen Toggle
          bindsym --no-repeat $mod+f fullscreen toggle

          # Window Layout Mode
          bindsym --no-repeat $mod+t layout toggle split tabbed

          # Window Position
          bindsym $mod+Shift+$left move left
          bindsym $mod+Shift+$down move down
          bindsym $mod+Shift+$up move up
          bindsym $mod+Shift+$right move right
          bindsym $mod+Shift+Left move left
          bindsym $mod+Shift+Down move down
          bindsym $mod+Shift+Up move up
          bindsym $mod+Shift+Right move right

        #
        # Navigate:
        #
          # Next Workspace
          bindsym $mod+Alt+Right workspace next

          # Previous Workspace
          bindsym $mod+Alt+Left workspace prev
          bindsym $mod+Shift+Tab workspace prev

          # Relative Window
          bindsym $mod+$left focus left
          bindsym $mod+$down focus down
          bindsym $mod+$up focus up
          bindsym $mod+$right focus right
          bindsym $mod+Left focus left
          bindsym $mod+Down focus down
          bindsym $mod+Up focus up
          bindsym $mod+Right focus right

          # Scratchpad
          bindsym --no-repeat $mod+Control+a scratchpad show

          # Window by Name
          bindsym --no-repeat $mod+Shift+space exec rofi -show window

          # Workspace 1-9
          bindsym --no-repeat $mod+1 workspace number 1
          bindsym --no-repeat $mod+2 workspace number 2
          bindsym --no-repeat $mod+3 workspace number 3
          bindsym --no-repeat $mod+4 workspace number 4
          bindsym --no-repeat $mod+5 workspace number 5
          bindsym --no-repeat $mod+6 workspace number 6
          bindsym --no-repeat $mod+7 workspace number 7
          bindsym --no-repeat $mod+8 workspace number 8
          bindsym --no-repeat $mod+9 workspace number 9
          # Workspace 10
          bindsym --no-repeat $mod+0 workspace number 10

        #
        # Resize:
        #
          # Enter Resize Mode
          bindsym $mod+r mode "resize"
          mode "resize" {
            # left will shrink the containers width
            # right will grow the containers width
            # up will shrink the containers height
            # down will grow the containers height
            bindsym $left resize shrink width 10px
            bindsym $down resize grow height 10px
            bindsym $up resize shrink height 10px
            bindsym $right resize grow width 10px
            # Ditto, with arrow keys
            bindsym Left resize shrink width 10px
            bindsym Down resize grow height 10px
            bindsym Up resize shrink height 10px
            bindsym Right resize grow width 10px

            # Return to default mode
            bindsym Return mode "default"
            bindsym Escape mode "default"
          }

        #
        # Session:
        #
          # Exit App
          bindsym --no-repeat $mod+Shift+q kill

          # Lock Screen
          bindsym --no-repeat $mod+Escape exec $lock

          # Logout
          bindsym --no-repeat $mod+Shift+e exec ${pkgs.sway}/bin/swaynag -t warning \
                  -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' \
                  -b 'Yes, exit sway' \
                  'swaymsg exit'

          # Power Down
          bindsym --no-repeat $mod+Shift+p exec ${pkgs.systemd}/bin/systemctl poweroff

          # Reboot
          bindsym --no-repeat $mod+Shift+b exec ${pkgs.systemd}/bin/systemctl reboot

          # TODO Refresh Session

          # Reload Sway Config
          bindsym --no-repeat $mod+Shift+c reload

          # TODO Restart Sway

          # Sleep
          bindsym --no-repeat $mod+Shift+s exec ${pkgs.systemd}/bin/systemctl suspend

          # TODO Terminate App

        #
        # Hotkeys:
        #
          bindsym XF86MonBrightnessDown exec ${pkgs.light}/bin/light -A 2
          bindsym XF86MonBrightnessUp exec ${pkgs.light}/bin/light -U 2

          bindsym XF86AudioRaiseVolume exec ${pkgs.pamixer}/bin/pamixer -ui 2
          bindsym XF86AudioLowerVolume exec ${pkgs.pamixer}/bin/pamixer -ud 2
          bindsym XF86AudioMute exec ${pkgs.pamixer}/bin/pamixer --toggle-mute

        client.focused #1f2937 #1f2937 #f3f4f6 #ffffff #ffffff
        client.focused_inactive #111827 #111827 #b3b3b3 #f3f4f6 #ffffff
        client.unfocused #111827 #111827 #b3b3b3 #f3f4f6 #ffffff
        client.urgent #111827 #111827 #b3b3b3 #f3f4f6 #ffffff
        default_border normal 2
        default_floating_border normal 2
        font pango:monospace 11
        gaps inner 16
        title_align center
        titlebar_border_thickness 0
        titlebar_padding 20 12

        set $ws_web 1
        set $ws_dev 2
        set $ws_media 3
        set $ws_im 4

        assign [app_id="Google-chrome"]               workspace $ws_web
        assign [class="Google-chrome"]                workspace $ws_web
        assign [app_id="firefox"]                     workspace $ws_web
        assign [class="Firefox"]                      workspace $ws_web
        assign [app_id="org.qutebrowser.qutebrowser"] workspace $ws_web
        assign [app_id="org.qbittorrent.qBittorrent"] workspace $ws_web
        assign [app_id="Alacritty"]                   workspace $ws_dev
        assign [app_id="emacs"]                       workspace $ws_dev
        assign [class="jetbrains-(.*)"]               workspace $ws_dev
        for_window [class="jetbrains-(.*)" title="win0"] floating enable
        assign [app_id="kitty"]                       workspace $ws_dev
        assign [app_id="org.zealdocs.Zeal"]           workspace $ws_dev
        assign [app_id="calibre-ebook-viewer"]        workspace $ws_media
        assign [app_id="calibre-edit-book"]           workspace $ws_media
        assign [app_id="calibre-gui"]                 workspace $ws_media
        assign [class="Gimp-2.10"]                    workspace $ws_media
        assign [class="krita"]                        workspace $ws_media
        assign [app_id="org.inkscape.Inkscape"]       workspace $ws_media
        assign [app_id="ristretto"]                   workspace $ws_media
        assign [app_id="thunar"]                      workspace $ws_media
        assign [class="Slack"]                        workspace $ws_im

        for_window [class="Bitwarden"]       floating enable
        for_window [app_id="gnome-pomodoro"] floating enable
        for_window [class="Todoist"]         floating enable

        include /etc/sway/config.d/*
        include /etc/sway/config.d/bindings/*
        include /etc/sway/config.d/startup/*
      '';
      "sway/config.d/10-systemd.conf".text = ''
        exec "systemctl --user import-environment; systemctl --user start sway-session.target"
      '';
      "sway/config.d/startup/dex.conf".text = ''
        exec ${pkgs.dex}/bin/dex -a
      '';
    };

    home.configFile."swaylock/config".source = "${configDir}/swaylock/config";

    modules.desktop.wayland.enable = true;

    modules.desktop.fontconfig.enable = true;
    modules.desktop.gtk.enable = true;
    modules.desktop.qt.enable = true;

    modules.desktop.fcitx5.enable = true;
    modules.desktop.apps = {
      dunst.enable = true;
      nwg-drawer.enable = true;
      rofi.enable = true;
      waybar.enable = true;
    };

    # TODO move this
    modules.term.theme.colorscheme = "doom-one";
  };
}

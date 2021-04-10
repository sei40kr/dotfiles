{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let
  cfg = config.modules.desktop.sway;
  nwg-launchers = config.modules.desktop.nwg-launchers.package;
in {
  options.modules.desktop.sway = with types; {
    enable = mkBoolOpt false;
    package = mkOption {
      type = package;
      default = null;
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    user = {
      extraGroups = [ "video" ];
      packages = with pkgs; [ wl-clipboard ];
    };
    home-manager.users.${config.user.name} = {
      wayland.windowManager.sway = {
        inherit (cfg) package;
        enable = true;
        config = {
          assigns = {
            "1" = [
              { window_role = "^browser$"; }
              { class = "^Chromium-browser$"; }
              { class = "^Google-chrome$"; }
              { class = "^qutebrowser$"; }
              { class = "^Geary$"; }
              { class = "^Gnome-calendar$"; }
              { class = "^Gnome-contacts$"; }
            ];
            "2" = [
              { class = "^Alacritty$"; }
              { class = "^Emacs$"; }
              { class = "^jetbrains-idea$"; }
              { class = "^VSCodium$"; }
              { class = "^Zeal$"; }
            ];
            "3" = [
              { class = "^File-roller$"; }
              { class = "^Evince$"; }
              { class = "^Eog$"; }
              { class = "^Org.gnome.Nautilus$"; }
              { class = "^Totem$"; }
            ];
            "4" = [
              { class = "^Skype$"; }
              { class = "^Slack$"; }
              { class = "^discord$"; }
            ];
          };
          bars = [ ];
          floating = {
            criteria = [
              { window_role = "^pop-up$"; }
              { class = "^Bitwarden$"; }
              { class = "^Dconf-editor$"; }
              { class = "^Fcitx-config-gtk3$"; }
              { class = "^Gcr-prompter$"; }
              { class = "^Gnome-control-center$"; }
              { class = "^Gnome-pomodoro$"; }
              { class = "^Gxmessage$"; }
              {
                class = "^jetbrains-";
                title = "^win0$";
              }
              { class = "^Xmessage$"; }
            ];
            titlebar = true;
          };
          focus.followMouse = "no";
          fonts = [ "sans-serif 10" ];
          keybindings = let mod = "Mod4";
          in {
            "${mod}+Return" =
              let terminal = config.modules.desktop.term.terminal;
              in mkIf (terminal != null) "exec ${terminal}";
            "${mod}+Shift+q" = "kill";
            "${mod}+h" = "focus left";
            "${mod}+j" = "focus down";
            "${mod}+k" = "focus up";
            "${mod}+l" = "focus right";
            "${mod}+Left" = "focus left";
            "${mod}+Down" = "focus down";
            "${mod}+Up" = "focus up";
            "${mod}+Right" = "focus right";
            "${mod}+Shift+h" = "move left";
            "${mod}+Shift+j" = "move down";
            "${mod}+Shift+u" = "move up";
            "${mod}+Shift+l" = "move right";
            "${mod}+Shift+Left" = "move left";
            "${mod}+Shift+Down" = "move down";
            "${mod}+Shift+Up" = "move up";
            "${mod}+Shift+Right" = "move right";
            "${mod}+b" = "splith";
            "${mod}+v" = "splitv";
            "${mod}+f" = "fullscreen toggle";
            "${mod}+a" = "focus parent";
            "${mod}+w" = "layout tabbed";
            "${mod}+e" = "layout toggle split";
            "${mod}+1" = "workspace number 1";
            "${mod}+2" = "workspace number 2";
            "${mod}+3" = "workspace number 3";
            "${mod}+4" = "workspace number 4";
            "${mod}+5" = "workspace number 5";
            "${mod}+6" = "workspace number 6";
            "${mod}+7" = "workspace number 7";
            "${mod}+8" = "workspace number 8";
            "${mod}+9" = "workspace number 9";
            "${mod}+Shift+1" =
              "move container to workspace number 1, workspace 1";
            "${mod}+Shift+2" =
              "move container to workspace number 2, workspace 2";
            "${mod}+Shift+3" =
              "move container to workspace number 3, workspace 3";
            "${mod}+Shift+4" =
              "move container to workspace number 4, workspace 4";
            "${mod}+Shift+5" =
              "move container to workspace number 5, workspace 5";
            "${mod}+Shift+6" =
              "move container to workspace number 6, workspace 6";
            "${mod}+Shift+7" =
              "move container to workspace number 7, workspace 7";
            "${mod}+Shift+8" =
              "move container to workspace number 8, workspace 8";
            "${mod}+Shift+9" =
              "move container to workspace number 9, workspace 9";
            "${mod}+Shift+minus" = "move scratchpad";
            "${mod}+minus" = "scratchpad show";
            "${mod}+Shift+c" = "reload";
            "${mod}+Shift+e" =
              "exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'";
            "${mod}+r" = "mode resize";

            "XF86MonBrightnessDown" =
              "exec ${pkgs.brightnessctl}/bin/brightnessctl set 2%-";
            "XF86MonBrightnessUp" =
              "exec ${pkgs.brightnessctl}/bin/brightnessctl set +4%";
            "XF86AudioRaiseVolume" =
              "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioLowerVolume" =
              "exec ${pkgs.pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioMute" =
              "exec ${pkgs.pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";

            # mako
            "${mod}+space" = "exec ${pkgs.mako}/bin/makoctl dismiss";
            "${mod}+Shift+space" = "exec ${pkgs.mako}/bin/makoctl dismiss -a";
            "${mod}+Shift+period" =
              "exec ${pkgs.mako}/bin/makoctl menu ${nwg-launchers}/bin/nwgdmenu -n";

            # grimshot
            "Print" =
              "exec ${pkgs.sway-contrib.grimshot}/bin/grimshot --notify copy screen";

            # nwggrid
            "${mod}+d" = "exec ${nwg-launchers}/bin/nwggrid";
          };
          window.titlebar = true;
        };
        extraConfig = ''
          exec "systemctl --user import-environment; systemctl --user start graphical-session.target"
          title_align center
          input "type:keyboard" {
            repeat_delay 150
            repeat_rate 30
          }
        '';
      };
      systemd.user.services = {
        sway-session = {
          Unit = {
            After = [ "graphical-session-pre.target" ];
            Description = "sway compositor session";
            Documentation = [ "man:systemd.special(7)" ];
            PartOf = [ "graphical-session.target" ];
            Wants = [ "swayidle.service" ];
          };
          Install = { WantedBy = [ "graphical-session.target" ]; };
        };
        swayidle = {
          Unit = {
            Description = "Idle manager for Wayland";
            Documentation = "man:swayidle(1)";
            PartOf = [ "sway-session.target" ];
          };
          Service = {
            Type = "simple";
            ExecStart = ''
              ${pkgs.swayidle}/bin/swayidle -w -d \
                  timeout 600 '${cfg.package}/bin/swaymsg "output * dpms off"' \
                  resume '${cfg.package}/bin/swaymsg "output * dpms on"'
            '';
          };
        };
      };
    };
    # TODO Use user-level Fontconfig
    fonts = {
      fontconfig = {
        enable = true;
        defaultFonts = {
          emoji = [ "Noto Color Emoji" ];
          monospace = [ "Noto Sans Mono" "Noto Sans Mono CJK JP" ];
          sansSerif = [ "Noto Sans" "Noto Sans CJK JP" ];
          serif = [ "Noto Serif" "Noto Serif CJK JP" ];
        };
        hinting.enable = false;
      };
      fonts = with pkgs; [ noto-fonts noto-fonts-cjk noto-fonts-emoji ];
    };
    hardware.opengl.enable = true;
    modules = {
      desktop = {
        sway.package = pkgs.sway.override {
          extraSessionCommands = concatStringsSep "\n"
            (mapAttrsToList (n: v: ''export ${n}="${v}"'')
              config.modules.desktop.env);
          withGtkWrapper = true;
        };
        env = {
          SDL_VIDEODRIVER = "wayland";
          # Needs qt5.qtwayland in systemPackages
          QT_QPA_PLATFORM = "wayland";
          QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
          # Fix for some Java AWT applications (e.g. Android Studio),
          # use this if they aren't displayed properly:
          _JAVA_AWT_WM_NONREPARENTING = "1";
          XDG_SESSION_TYPE = "wayland";
        };
        wayland = true;

        dconf.enable = true;
        fcitx.enable = true;
        gammastep.enable = true;
        gtk.enable = true;
        mako.enable = true;
        nwg-launchers.enable = true;
        waybar.enable = true;
        wofi.enable = true;
      };
      services.random-background.enable = true;
    };
  };
}

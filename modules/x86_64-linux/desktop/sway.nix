{ config, home-manager, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.sway;
in {
  options.modules.desktop.sway = with types; {
    enable = mkBoolOpt false;
    theme.background.imageDirectory = mkOpt (either path str) null;
  };

  config = mkIf cfg.enable {
    user = {
      extraGroups = [ "video" ];
      packages = with pkgs; [ wl-clipboard ];
    };
    home-manager.users.${config.user.name} = {
      wayland.windowManager.sway = {
        enable = true;
        config = with pkgs; {
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
            "${mod}+s" = "layout stacking";
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
              "exec ${brightnessctl}/bin/brightnessctl set 2%-";
            "XF86MonBrightnessUp" =
              "exec ${brightnessctl}/bin/brightnessctl set +4%";
            "XF86AudioRaiseVolume" =
              "exec ${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ +5%";
            "XF86AudioLowerVolume" =
              "exec ${pulseaudio}/bin/pactl set-sink-volume @DEFAULT_SINK@ -5%";
            "XF86AudioMute" =
              "exec ${pulseaudio}/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle";

            # mako
            "${mod}+space" = "exec ${mako}/bin/makoctl dismiss";
            "${mod}+Shift+space" = "exec ${mako}/bin/makoctl dismiss -a";
            "${mod}+Shift+period" =
              "exec ${mako}/bin/makoctl menu wofi -dp 'Choose Action'";

            # grimshot
            "Print" =
              "exec ${sway-contrib.grimshot}/bin/grimshot --notify copy screen";

            # wofi
            "${mod}+d" =
              "exec ${wofi}/bin/wofi -S drun -p 'Search Applications'";
          };
          startup = let
            random-backgrounds = writeShellScriptBin "random-backgrounds" ''
              IMAGE_DIRECTORY=${
                escapeShellArg cfg.theme.background.imageDirectory
              }

              shopt -s nullglob
              cd "$IMAGE_DIRECTORY"
              while true; do
                for file in *.jpg *.png; do
                  if [[ -f "$file" ]]; then
                    files+=("$file")
                  fi
                done
                range="''${#files[@]}"
                swaybg -i "''${files[RANDOM % range]}" -m fill -o '*' &
                pid="$!"
                sleep 1h
                kill "$pid"
              done
            '';
          in [
            {
              command = ''
                ${swayidle}/bin/swayidle -w -d \
                    timeout 600 '${sway}/bin/swaymsg "output * dpms off"' \
                    resume '${sway}/bin/swaymsg "output * dpms on"'
              '';
            }
            { command = "${random-backgrounds}/bin/random-backgrounds"; }
            { command = "${mako}/bin/mako"; }
          ];
          window.titlebar = true;
        };
        extraConfig = ''
          title_align center
          input "type:keyboard" {
            repeat_delay 150
            repeat_rate 30
          }
        '';
        extraSessionCommands = ''
          export SDL_VIDEODRIVER='wayland'
          # Needs qt5.qtwayland in systemPackages
          export QT_QPA_PLATFORM='wayland'
          export QT_WAYLAND_DISABLE_WINDOWDECORATION='1'
          # Fix for some Java AWT applications (e.g. Android Studio),
          # use this if they aren't displayed properly:
          export _JAVA_AWT_WM_NONREPARENTING='1'
        '';
        systemdIntegration = true;
        wrapperFeatures.gtk = true;
      };
      gtk = {
        enable = true;
        # Disable mouse paste
        gtk3.extraConfig.gtk-enable-primary-paste = false;
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
    modules.desktop = {
      dconf.enable = true;
      fcitx.enable = true;
      gammastep.enable = true;
      mako.enable = true;
      waybar.enable = true;
      wofi.enable = true;
    };
  };
}
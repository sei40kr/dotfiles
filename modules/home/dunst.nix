{
  config,
  inputs,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib)
    genAttrs
    mdDoc
    mkEnableOption
    mkIf
    mkOption
    removePrefix
    types
    ;
  inherit (inputs.self.lib.extraTypes) fontType;
  inherit (types)
    enum
    int
    nullOr
    package
    str
    submodule
    ;
  cfg = config.modules.desktop.apps.dunst;
  wmCfg = osConfig.modules.desktop.wm;
  deCfg = osConfig.modules.desktop.de;
  anyrunCfg = config.modules.desktop.apps.anyrun;

  separatorType = submodule {
    options = {
      height = mkOption {
        type = int;
        default = 2;
        description = mdDoc ''
          The height of the separator between notification windows.
        '';
      };

      color = mkOption {
        type = str;
        default = cfg.border.color;
        description = mdDoc ''
          The color of the separator between notification windows.
        '';
      };
    };
  };

  playMessageSound = pkgs.writeShellScript "pw-play-message-sound" ''
    ${pkgs.pipewire}/bin/pw-play --media-role=Notification ${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/message.oga
  '';
  playCompleteSound = pkgs.writeShellScript "pw-play-complete-sound" ''
    ${pkgs.pipewire}/bin/pw-play --media-role=Notification ${pkgs.sound-theme-freedesktop}/share/sounds/freedesktop/stereo/complete.oga
  '';
in
{
  options.modules.desktop.apps.dunst = {
    enable = mkEnableOption (mdDoc ''
      Whether to enable Dunst.
    '');

    width = mkOption {
      type = int;
      default = 300;
      description = mdDoc ''
        The width in pixels of a notification window.
      '';
    };

    height = mkOption {
      type = int;
      default = 300;
      description = mdDoc ''
        The height in pixels of a single notification window.
      '';
    };

    position = {
      origin = mkOption {
        type = enum [
          "top-left"
          "top-center"
          "top-right"
          "bottom-left"
          "bottom-center"
          "bottom-right"
          "left-center"
          "center"
          "right-center"
        ];
        default = "top-right";
        description = mdDoc ''
          The origin of the notifications.
        '';
      };

      offset = {
        x = mkOption {
          type = int;
          default = wmCfg.gaps.outer;
          description = mdDoc ''
            The horizontal offset from the origin.
          '';
        };

        y = mkOption {
          type = int;
          default = wmCfg.gaps.outer;
          description = mdDoc ''
            The vertical offset from the origin.
          '';
        };
      };
    };

    separator = mkOption {
      type = nullOr separatorType;
      default = null;
      description = mdDoc ''
        The separator between notification windows. If set to null, no
        separator will be shown.
      '';
    };

    padding = {
      x = mkOption {
        type = int;
        default = 8;
        description = mdDoc ''
          The horizontal padding of a notification window.
        '';
      };

      y = mkOption {
        type = int;
        default = 8;
        description = mdDoc ''
          The vertical padding of a notification window.
        '';
      };

      textIcon = mkOption {
        type = int;
        default = 0;
        description = mdDoc ''
          The padding between the text and the icon.
        '';
      };
    };

    borderWidth = mkOption {
      type = int;
      default = 3;
      description = mdDoc ''
        The width of the border around a notification window.
      '';
    };

    gap = mkOption {
      type = int;
      default = wmCfg.gaps.inner;
      description = mdDoc ''
        The size of the gap between notifications.
      '';
    };

    font = mkOption {
      type = fontType;
      default = deCfg.defaultFonts.ui;
      description = mdDoc ''
        The font to use for the notifications.
      '';
    };

    lineHeight = mkOption {
      type = nullOr int;
      default = null;
      description = mdDoc ''
        The spacing between lines. If set to null, the height will be
        automatically raised to the font height.
      '';
    };

    alignments = {
      horizontal = mkOption {
        type = enum [
          "left"
          "center"
          "right"
        ];
        default = "left";
        description = mdDoc ''
          The horizontal alignment of the text.
        '';
      };

      vertical = mkOption {
        type = enum [
          "top"
          "center"
          "bottom"
        ];
        default = "center";
        description = mdDoc ''
          The vertical alignment of the text and icon.
        '';
      };
    };

    icon = {
      position = mkOption {
        type = nullOr (enum [
          "left"
          "right"
          "top"
        ]);
        default = "left";
        description = mdDoc ''
          The position of the icon. If set to null, the icon will not be shown.
        '';
      };

      size = {
        min = mkOption {
          type = int;
          default = 32;
          description = mdDoc ''
            The minimum size of the icon.
          '';
        };

        max = mkOption {
          type = int;
          default = 128;
          description = mdDoc ''
            The maximum size of the icon.
          '';
        };
      };
    };

    cornerRadius = mkOption {
      type = int;
      default = 0;
      description = mdDoc ''
        The corner radius of a notification window.
      '';
    };

    low = {
      background = mkOption {
        type = str;
        default = cfg.normal.background;
        description = mdDoc ''
          The background color of a notification window.
        '';
      };

      foreground = mkOption {
        type = str;
        default = cfg.normal.foreground;
        description = mdDoc ''
          The foreground color of a notification window.
        '';
      };

      timeout = mkOption {
        type = int;
        default = 5;
        description = mdDoc ''
          The timeout in seconds of a notification window.
        '';
      };

      borderColor = mkOption {
        type = str;
        default = cfg.normal.borderColor;
        description = mdDoc ''
          The color of the border around a notification window.
        '';
      };
    };

    normal = {
      background = mkOption {
        type = str;
        example = "#222222";
        description = mdDoc ''
          The background color of a notification window.
        '';
      };

      foreground = mkOption {
        type = str;
        example = "#888888";
        description = mdDoc ''
          The foreground color of a notification window.
        '';
      };

      timeout = mkOption {
        type = int;
        default = 0;
        description = mdDoc ''
          The timeout in seconds of a notification window.
        '';
      };

      borderColor = mkOption {
        type = str;
        default = "#00000000";
        description = mdDoc ''
          The color of the border around a notification window.
        '';
      };
    };

    critical = {
      background = mkOption {
        type = str;
        default = cfg.normal.background;
        description = mdDoc ''
          The background color of a notification window.
        '';
      };

      foreground = mkOption {
        type = str;
        default = cfg.normal.foreground;
        description = mdDoc ''
          The foreground color of a notification window.
        '';
      };

      timeout = mkOption {
        type = int;
        default = 0;
        description = mdDoc ''
          The timeout in seconds of a notification window.
        '';
      };

      borderColor = mkOption {
        type = str;
        default = cfg.normal.borderColor;
        description = mdDoc ''
          The color of the border around a notification window.
        '';
      };
    };

    package = mkOption {
      type = package;
      default = pkgs.dunst;
      description = mdDoc ''
        The package to use for Dunst.
      '';
      visible = false;
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.separator == null || cfg.gap == 0;
        message = "The separator must be null unless the gap is 0.";
      }
    ];

    services.dunst = {
      enable = true;
      settings = {
        global = {
          monitor = 0;
          follow = "mouse";
          inherit (cfg) width height;
          inherit (cfg.position) origin;
          offset = "${toString cfg.position.offset.x}x${toString cfg.position.offset.y}";
          scale = 0;
          notification_limit = 0;
          progress_bar = false;
          indicate_hidden = "yes";
          separator_height = if cfg.separator != null then cfg.separator.height else 0;
          padding = cfg.padding.y;
          horizontal_padding = cfg.padding.x;
          text_icon_padding = cfg.padding.textIcon;
          frame_width = cfg.borderWidth;
          gap_size = cfg.gap;
          separator_color = if cfg.separator != null then ''"${cfg.separator.color}"'' else "";
          sort = "yes";
          font = "${cfg.font.name} ${toString cfg.font.size}";
          line_height = if cfg.lineHeight != null then cfg.lineHeight else 0;
          markup = "full";
          format = "<b>%s</b>\\n%b";
          alignment = cfg.alignments.horizontal;
          vertical_alignment = cfg.alignments.vertical;
          show_age_threshold = 60;
          ellipsize = "end";
          ignore_newline = "no";
          stack_duplicates = true;
          hide_duplicate_count = false;
          show_indicators = "no";
          enable_recursive_icon_lookup = true;
          icon_theme = config.gtk.iconTheme.name;
          icon_position = if cfg.icon.position != null then cfg.icon.position else "off";
          min_icon_size = cfg.icon.size.min;
          max_icon_size = cfg.icon.size.max;
          sticky_history = "yes";
          history_length = 20;
          dmenu = if anyrunCfg.enable then "anyrun-dmenu" else "true";
          browser = "${pkgs.xdg-utils}/bin/xdg-open";
          always_run_script = true;
          title = "Dunst";
          class = "Dunst";
          corner_radius = cfg.cornerRadius;
          ignore_dbusclose = false;
          force_xwayland = false;
          force_xinerama = false;
          mouse_left_click = "context";
          mouse_middle_click = "none";
          mouse_right_click = "close_current";
        };

        experimental = {
          per_monitor_dpi = true;
        };

        play_message_sound_everything = {
          script = "${playMessageSound}";
        };

        claude_code_complete = {
          summary = "Claude Code";
          body = "Response complete!";
          script = "${playCompleteSound}";
        };
      }
      // (genAttrs
        [
          "urgency_low"
          "urgency_normal"
          "urgency_critical"
        ]
        (
          name:
          let
            level = removePrefix "urgency_" name;
          in
          {
            inherit (cfg.${level}) background foreground timeout;
            frame_color = cfg.${level}.borderColor;
          }
        )
      );
    };
  };
}

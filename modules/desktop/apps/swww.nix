{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    escapeShellArg
    mkEnableOption
    mkIf
    removePrefix
    ;

  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  cfg = desktopCfg.apps.swww;

  bgColor = removePrefix "#" deCfg.background.color;

  resizeArg =
    if deCfg.background.image == null then
      null
    else if deCfg.background.image.mode == "fill" then
      "--resize crop"
    else if deCfg.background.image.mode == "fit" then
      "--resize fit"
    else if deCfg.background.image.mode == "center" then
      "--no-resize"
    else
      throw "Unsupported background mode '${deCfg.background.image.mode}' for swww. Supported modes: fill, fit, center";

  imgCommand =
    if deCfg.background.image != null then
      "${pkgs.swww}/bin/swww img ${escapeShellArg deCfg.background.image.path} ${resizeArg}"
    else
      "${pkgs.swww}/bin/swww clear ${bgColor}";
in
{
  options.modules.desktop.apps.swww = {
    enable = mkEnableOption "swww (awww)";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [ swww ];

    systemd.user.services.swww-daemon = {
      description = "swww daemon";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.swww}/bin/swww-daemon";
        Restart = "on-failure";
      };
    };

    systemd.user.services.swww = {
      description = "Set wallpaper with awww";
      wantedBy = [ "graphical-session.target" ];
      partOf = [ "graphical-session.target" ];
      requires = [ "swww-daemon.service" ];
      serviceConfig.Type = "oneshot";
      script = ''
        if [ -n "$WAYLAND_DISPLAY" ]; then
          SOCKET_NAME=$(basename "$WAYLAND_DISPLAY")
        else
          SOCKET_NAME="wayland-0"
        fi

        SOCKET_PATH="$XDG_RUNTIME_DIR/$SOCKET_NAME-swww-daemon..sock"

        # Wait for socket to be created (max 10 seconds)
        for i in {1..100}; do
          if [ -S "$SOCKET_PATH" ]; then
            break
          fi
          sleep 0.1
        done

        if [ ! -S "$SOCKET_PATH" ]; then
          echo "swww-daemon socket not found after 10 seconds at $SOCKET_PATH" >&2
          exit 1
        fi

        ${imgCommand}
      '';
    };
  };
}

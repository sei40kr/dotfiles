{
  config,
  lib,
  osConfig,
  pkgs,
  ...
}:

let
  inherit (lib)
    attrByPath
    escapeShellArg
    mkEnableOption
    mkIf
    removePrefix
    ;

  cfg = config.modules.desktop.apps.awww;
  deCfg = attrByPath [ "modules" "desktop" "de" ] {
    background = {
      color = "#000000";
      image = null;
    };
  } osConfig;

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
      throw "Unsupported background mode '${deCfg.background.image.mode}' for awww. Supported modes: fill, fit, center";

  imgCommand =
    if deCfg.background.image != null then
      "${pkgs.awww}/bin/awww img ${escapeShellArg deCfg.background.image.path} ${resizeArg}"
    else
      "${pkgs.awww}/bin/awww clear ${bgColor}";
in
{
  options.modules.desktop.apps.awww = {
    enable = mkEnableOption "awww";
  };

  config = mkIf cfg.enable {
    services.awww.enable = true;

    systemd.user.services.awww-set-wallpaper = {
      Unit = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = "Set wallpaper with awww";
        PartOf = [ config.wayland.systemd.target ];
        Requires = "awww.service";
      };
      Service = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "awww-start" ''
          if [ -n "$WAYLAND_DISPLAY" ]; then
            SOCKET_NAME=$(basename "$WAYLAND_DISPLAY")
          else
            SOCKET_NAME="wayland-0"
          fi

          SOCKET_PATH="$XDG_RUNTIME_DIR/$SOCKET_NAME-awww-daemon.sock"

          # Wait for socket to be created (max 10 seconds)
          for i in {1..100}; do
            if [ -S "$SOCKET_PATH" ]; then
              break
            fi
            sleep 0.1
          done

          if [ ! -S "$SOCKET_PATH" ]; then
            echo "awww-daemon socket not found after 10 seconds at $SOCKET_PATH" >&2
            exit 1
          fi

          ${imgCommand}
        '';
      };
      Install = {
        WantedBy = [ config.wayland.systemd.target ];
      };
    };
  };
}

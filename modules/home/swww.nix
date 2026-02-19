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

  cfg = config.modules.desktop.apps.swww;
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
      throw "Unsupported background mode '${deCfg.background.image.mode}' for swww. Supported modes: fill, fit, center";

  imgCommand =
    if deCfg.background.image != null then
      "${pkgs.swww}/bin/swww img ${escapeShellArg deCfg.background.image.path} ${resizeArg}"
    else
      "${pkgs.swww}/bin/swww clear ${bgColor}";
in
{
  options.modules.desktop.apps.swww = {
    enable = mkEnableOption "swww";
  };

  config = mkIf cfg.enable {
    services.swww.enable = true;

    systemd.user.services.swww-set-wallpaper = {
      Unit = {
        ConditionEnvironment = "WAYLAND_DISPLAY";
        Description = "Set wallpaper with swww";
        PartOf = [ config.wayland.systemd.target ];
        Requires = "swww.service";
      };
      Service = {
        Type = "oneshot";
        ExecStart = pkgs.writeShellScript "swww-start" ''
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
      Install = {
        WantedBy = [ config.wayland.systemd.target ];
      };
    };
  };
}

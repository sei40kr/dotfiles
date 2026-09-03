{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib)
    attrNames
    concatStringsSep
    filterAttrs
    head
    length
    mapAttrsToList
    mkEnableOption
    mkIf
    optionalAttrs
    ;

  cfg = config.modules.desktop.noctalia-greeter;
  deCfg = config.modules.desktop.de;
  wmCfg = config.modules.desktop.wm;

  enabledMonitors = filterAttrs (_: monitor: monitor.enable) wmCfg.monitors;
  enabledMonitorNames = attrNames enabledMonitors;
in
{
  imports = [ inputs.noctalia-greeter.nixosModules.default ];

  options.modules.desktop.noctalia-greeter = {
    enable = mkEnableOption "Noctalia Greeter";
  };

  config = mkIf cfg.enable {
    programs.noctalia-greeter = {
      enable = true;
      settings = {
        session.default = mkIf wmCfg.niri.enable "Niri";
        appearance = {
          # Match the Noctalia Shell theme (see modules/home/noctalia).
          scheme = "Tokyo-Night";
          theme_mode = "dark";
          font_family = deCfg.defaultFonts.ui.name;
          wallpaper =
            if deCfg.background.image != null then
              {
                path = deCfg.background.image.path;
                fill_mode = deCfg.background.image.mode;
              }
            else
              {
                path = "color:${deCfg.background.color}";
              };
        };
        output = mkIf (enabledMonitors != { }) (
          {
            scales = concatStringsSep "; " (
              mapAttrsToList (name: monitor: "${name}:${toString monitor.scale}") enabledMonitors
            );
          }
          # Unpinned, the greeter mirrors on every connected output and may put
          # cursor/focus on a disabled monitor.
          // optionalAttrs (length enabledMonitorNames == 1) { name = head enabledMonitorNames; }
        );
      };
    };

    # The greeter discovers sessions in /run/current-system/sw/share/wayland-sessions.
    environment.pathsToLink = [ "/share/wayland-sessions" ];
  };
}

{
  config,
  inputs,
  lib,
  ...
}:

let
  inherit (lib)
    concatStringsSep
    mapAttrsToList
    mkEnableOption
    mkIf
    optionalString
    ;
  inherit (builtins) toString;

  desktopCfg = config.modules.desktop;
  deCfg = desktopCfg.de;
  wmCfg = desktopCfg.wm;
  cfg = wmCfg.niri;

  rotationToTransform =
    { degrees, flipped }:
    if flipped then
      if degrees == 0 then "flipped" else "flipped-${toString degrees}"
    else if degrees == 0 then
      "normal"
    else
      toString degrees;

  outputsConfig = concatStringsSep "\n" (
    mapAttrsToList (
      name:
      {
        enable,
        resolution,
        refreshRate,
        scale,
        rotation,
        position,
        vrr,
        ...
      }:
      ''
        output "${name}" {
          ${optionalString (!enable) "off"}
          ${optionalString (resolution != null) ''
            mode "${toString resolution.width}x${toString resolution.height}${
              optionalString (refreshRate != null) "@${toString refreshRate}"
            }"
          ''}
          ${optionalString (scale != 1.0) "scale ${toString scale}"}
          ${optionalString (
            rotation.degrees != 0 || rotation.flipped
          ) ''transform "${rotationToTransform rotation}"''}
          ${optionalString (position != null) "position x=${toString position.x} y=${toString position.y}"}
          ${optionalString vrr "variable-refresh-rate"}
        }
      ''
    ) deCfg.monitors
  );
in
{
  imports = [
    inputs.self.nixosModules.de
    inputs.self.nixosModules.wm
  ];

  options.modules.desktop.wm.niri = {
    enable = mkEnableOption "Niri window manager";
  };

  config = mkIf cfg.enable {
    programs.niri.enable = true;

    environment.etc."niri/config.kdl".text = ''
      input {
        keyboard {
          repeat-delay ${toString deCfg.autoRepeat.delay}
          repeat-rate ${toString deCfg.autoRepeat.interval}
        }
      }

      ${outputsConfig}

      layout {
        gaps ${toString wmCfg.gaps.inner}

        struts {
          left ${toString ((wmCfg.gaps.outer - wmCfg.gaps.inner) * 2)}
          right ${toString ((wmCfg.gaps.outer - wmCfg.gaps.inner) * 2)}
          top ${toString (wmCfg.gaps.outer - wmCfg.gaps.inner)}
          bottom ${toString (wmCfg.gaps.outer - wmCfg.gaps.inner)}
        }

        background-color "${deCfg.background.color}"
      }

      include "${inputs.self}/config/niri/config.kdl"
    '';

    environment.etc."nvidia/nvidia-application-profiles-rc.d/50-limit-free-buffer-pool-in-wayland-compositors.json" =
      mkIf config.hardware.nvidia.enabled {
        text = builtins.toJSON {
          rules = [
            {
              pattern = {
                feature = "procname";
                matches = "niri";
              };
              profile = "Limit Free Buffer Pool On Wayland Compositors";
            }
          ];
          profiles = [
            {
              name = "Limit Free Buffer Pool On Wayland Compositors";
              settings = [
                {
                  key = "GLVidHeapReuseRatio";
                  value = 0;
                }
              ];
            }
          ];
        };
      };

    modules.desktop.de.enable = true;
    modules.desktop.de.wayland = true;
  };
}

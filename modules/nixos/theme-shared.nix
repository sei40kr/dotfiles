{
  lib,
  ...
}:

let
  inherit (lib) mdDoc mkOption types;
  inherit (types) enum nullOr;
in
{
  options.modules.desktop.theme = {
    active = mkOption {
      type = nullOr (enum [
        "graphite"
        "whitesur"
        "orchis"
      ]);
      default = null;
      description = mdDoc ''
        The active theme.
      '';
    };
  };
}

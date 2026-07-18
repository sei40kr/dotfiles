{
  config,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.rugit;
in
{
  options.modules.dev.tools.rugit = {
    enable = mkEnableOption "rugit";
  };

  config = mkIf cfg.enable {
    home.packages = [ perSystem.rugit.default ];
  };
}

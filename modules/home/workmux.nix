{
  config,
  inputs,
  lib,
  perSystem,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.workmux;
in
{
  options.modules.dev.tools.workmux = {
    enable = mkEnableOption "workmux";
  };

  config = mkIf cfg.enable {
    home.packages = [
      perSystem.workmux.default
    ];

    home.shellAliases = {
      wm = "workmux";
    };

    modules.ai.skillPaths = [
      "${inputs.workmux}/skills"
    ];

    modules.ai.permissions.allowedCommandPrefixes = [
      "workmux list"
      "workmux ls"
      "workmux open"
      "workmux dashboard"
      "workmux config"
    ];
  };
}

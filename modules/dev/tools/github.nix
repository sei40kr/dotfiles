{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.dev.tools.github;
in
{
  options.modules.dev.tools.github = {
    enable = mkEnableOption "GitHub tools";
  };

  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      gh
      actionlint
      pinact
    ];
  };
}

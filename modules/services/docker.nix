{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    compose.enable = mkEnableOption "Compose";
  };

  config = mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };

    environment.systemPackages = with pkgs; [
      docker-credential-helpers
      (mkIf cfg.compose.enable docker-compose)
    ];

    user.extraGroups = [ "docker" ];
  };
}

{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib)
    mkEnableOption
    mkIf
    ;
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkEnableOption "Docker";

    compose.enable = mkEnableOption "Docker Compose";
  };

  config = mkIf cfg.enable {
    virtualisation.docker = {
      enable = true;
      autoPrune.enable = true;
    };

    environment.systemPackages =
      with pkgs;
      [
        docker-credential-helpers
      ]
      ++ lib.optional cfg.compose.enable docker-compose;

    users.users.sei40kr.extraGroups = [ "docker" ];
  };
}

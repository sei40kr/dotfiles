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
    mkOption
    types
    ;
  cfg = config.modules.services.docker;
in
{
  options.modules.services.docker = {
    enable = mkEnableOption "Docker container runtime";

    compose.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable Docker Compose";
    };
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

    # Add user to docker group
    # Note: This assumes the user 'sei40kr' exists, configured in host configuration
    users.users.sei40kr.extraGroups = [ "docker" ];
  };
}

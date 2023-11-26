{ config, lib, ... }:

with lib;
with lib.my;
{
  imports = mapModulesRec' (toString ./.) import;

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";

  home-manager.users.${config.user.name}.home.stateVersion = config.system.stateVersion;
}

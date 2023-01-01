{ config, inputs, lib, ... }:

with lib;
with lib.my;
{
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
}

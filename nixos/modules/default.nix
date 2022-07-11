{ inputs, lib, ... }:

with lib;
with lib.my;
let users = [ "root" config.user.name ];
in
{
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  nix.settings = {
    trusted-users = users;
    allowed-users = users;
  };

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
}

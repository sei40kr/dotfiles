{ inputs, lib, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  system.stateVersion = "20.09";

  i18n.extraLocaleSettings.LC_CTYPE = "ja_JP.UTF-8";
}

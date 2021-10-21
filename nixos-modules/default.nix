{ inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./.) import);

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  system.stateVersion = "20.09";

  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_5_10;
  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  i18n.extraLocaleSettings.LC_CTYPE = "ja_JP.UTF-8";
}

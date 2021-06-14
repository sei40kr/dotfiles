{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ inputs.home-manager.nixosModules.home-manager ]
    ++ (mapModulesRec' (toString ./modules/common) import)
    ++ (mapModulesRec' (toString ./modules/nixos) import);

  environment.variables.NIXPKGS_ALLOW_UNFREE = "1";
  nix = {
    package = pkgs.nixFlakes;
    extraOptions = "experimental-features = nix-command flakes";
  };
  system.stateVersion = "20.09";

  # Use the latest kernel
  boot.kernelPackages = pkgs.linuxPackages_5_10;
  # Use the systemd-boot EFI boot loader.
  boot.loader = {
    efi.canTouchEfiVariables = true;
    systemd-boot.enable = true;
  };

  environment.systemPackages = with pkgs; [ coreutils git gnumake vim ];

  user.home = "/home/${config.user.name}";
}

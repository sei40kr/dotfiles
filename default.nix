device: username:
{ config, lib, options, pkgs, ... }:

{
  imports = [ ./modules ./secrets.nix ./hosts/my-server.nix ];

  nix.autoOptimiseStore = true;
  nixpkgs = {
    overlays = import ./packages;
    config.allowUnfree = true;
  };

  environment.systemPackages = with pkgs; [
    binutils
    coreutils
    diffutils
    findutils
    vim
  ];
}

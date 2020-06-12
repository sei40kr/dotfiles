device: username:
{ config, lib, options, pkgs, ... }:

{
  imports = [ ./modules ./hosts/my-server.nix ];

  nix.autoOptimiseStore = true;
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    binutils
    coreutils
    diffutils
    findutils
  ];
}

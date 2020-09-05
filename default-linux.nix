_: _:
{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    <home-manager/nixos>

    ./modules
    ./modules-linux
    ./secrets.nix
    ./per-host/my-server.nix
  ];

  options.my.home =
    mkOption { type = options.home-manager.users.type.functor.wrapped; };

  config = {
    nix.autoOptimiseStore = true;

    nixpkgs = {
      config.allowUnfree = true;
      overlays = import ./packages;
    };

    home-manager.users.${config.my.userName} =
      mkAliasDefinitions options.my.home;

    environment.systemPackages = with pkgs; [
      binutils
      coreutils
      diffutils
      findutils
      vim
    ];
  };
}

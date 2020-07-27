{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    <home-manager/nix-darwin>

    ./modules
    ./hosts/my-work-pc.nix
  ];

  options.my.home =
    mkOption { type = options.home-manager.users.type.functor.wrapped; };

  config = {
    nix.useDaemon = true;

    nixpkgs = {
      config.allowUnfree = true;
      overlays = import ./packages;
    };

    home-manager.users.${config.my.userName} =
      mkAliasDefinitions options.my.home;
  };
}

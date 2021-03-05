{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    <home-manager/nix-darwin>

    ./modules
    ./modules-darwin
    ./hosts/RLSUU178967M01.local.nix
  ];

  options.my.home =
    mkOption { type = options.home-manager.users.type.functor.wrapped; };

  config = {
    nix.useDaemon = true;

    nixpkgs = {
      config.allowUnfree = true;
      overlays = import ./packages;
    };

    # Configure nix-darwin to install the user applications
    system.build.applications = pkgs.buildEnv {
      name = "user-applications";
      paths = config.user.packages;
      pathsToLink = "/Applications";
    };

    # Configure nix-darwin to copy the files under $out/share/fonts to
    # ~/Library/Fonts
    # fonts = {
    #   enableFontDir = true;
    #   fonts = config.modules.editors.fonts.packages;
    # };
  };
}

{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    <home-manager/nix-darwin>

    ./modules
    ./modules-darwin
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

    # Configure nix-darwin to install the user applications
    system.build.applications = pkgs.buildEnv {
      name = "user-applications";
      paths = config.my.packages;
      pathsToLink = "/Applications";
    };

    # Configure nix-darwin to copy the files under $out/share/fonts to
    # ~/Library/Fonts
    fonts = {
      enableFontDir = true;
      fonts = config.modules.dev.editors.fonts.packages;
    };

    home-manager.users.${config.my.userName} =
      mkAliasDefinitions options.my.home;
  };
}

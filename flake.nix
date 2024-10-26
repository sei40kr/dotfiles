{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-24.05";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        home-manager.follows = "home-manager";
      };
    };

    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    flake-parts.url = "github:hercules-ci/flake-parts";

    idea-doom-emacs = {
      url = "github:sei40kr/idea-doom-emacs";
      flake = false;
    };

    lazyvim = {
      url = "github:sei40kr/nix-lazyvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tmux-project = {
      url = "github:sei40kr/tmux-project";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-tmux = {
      url = "github:sei40kr/wez-tmux";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-pain-control = {
      url = "github:sei40kr/wez-pain-control";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-per-project-workspace = {
      url = "github:sei40kr/wez-per-project-workspace";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    wez-status-generator = {
      url = "github:sei40kr/wez-status-generator";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      agenix,
      fenix,
      flake-parts,
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      ...
    }@inputs:
    let
      inherit (flake-parts.lib) mkFlake;
      lib = nixpkgs.lib.extend (lib: _: { my = self.lib; });
      inherit (lib) attrValues;
      inherit (lib.my) mapModules;
    in
    mkFlake { inherit inputs; } (
      { withSystem, ... }:
      let
        nixosSystem =
          system: hostCfg:
          withSystem system (
            { inputs', pkgs, ... }:
            nixpkgs.lib.nixosSystem {
              inherit system;
              specialArgs = {
                inherit
                  inputs
                  inputs'
                  lib
                  pkgs
                  ;
              };
              modules = [
                agenix.nixosModules.default
                home-manager.nixosModules.home-manager
                ./modules
                hostCfg
              ];
            }
          );
      in
      {
        flake = {
          lib = import ./lib { inherit inputs lib; };

          overlays = mapModules ./overlays import;

          nixosConfigurations = mapModules ./hosts (path: import path { inherit nixosSystem; });
        };

        systems = [ "x86_64-linux" ];

        perSystem =
          {
            inputs',
            pkgs,
            self',
            system,
            ...
          }:
          {
            config._module.args.pkgs =
              let
                pkgs' = import nixpkgs-unstable {
                  inherit system;
                  config.allowUnfree = true;
                };
                pkgs = import nixpkgs {
                  inherit system;
                  config.allowUnfree = true;
                  overlays = [
                    (_: _: {
                      unstable = pkgs';
                      my = self'.packages;
                    })
                    (_: _: { agenix = inputs'.agenix.packages.default; })
                    fenix.overlays.default
                    (_: _: { wez-tmux = inputs'.wez-tmux.packages.default; })
                    (_: _: { wez-pain-control = inputs'.wez-pain-control.packages.default; })
                    (_: _: { wez-per-project-workspace = inputs'.wez-per-project-workspace.packages.default; })
                    (_: _: { wez-status-generator = inputs'.wez-status-generator.packages.default; })
                  ] ++ attrValues self.overlays;
                };
              in
              pkgs;

            config.packages = pkgs.callPackage ./packages {
              tmux-project = inputs'.tmux-project.packages.default;
            };

            config.devShells = pkgs.callPackage ./shells { };
          };
      }
    );
}

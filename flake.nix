{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/23.11";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager/release-23.11";
      inputs.nixpkgs.follows = "nixpkgs";
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

    nil = {
      url = "github:oxalica/nil";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    swayfx = {
      url = "github:WillPower3309/swayfx";
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

    yonvim = {
      url = "github:sei40kr/yonvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { self
    , nix-darwin
    , fenix
    , flake-parts
    , home-manager
    , nil
    , nixpkgs
    , nixpkgs-unstable
    , swayfx
    , ...
    }@inputs:
    let
      inherit (flake-parts.lib) mkFlake;
      lib = nixpkgs.lib.extend (lib: _: { my = self.lib; });
      inherit (lib) attrValues;
      inherit (lib.my) mapModules;
    in
    mkFlake { inherit inputs; } ({ withSystem, ... }:
    let
      nixosSystem = system: hostCfg: withSystem system ({ pkgs, ... }:
        nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs lib pkgs; };
          modules = [
            home-manager.nixosModules.home-manager
            ./modules
            ./nixos/modules
            hostCfg
          ];
        });

      darwinSystem = system: hostCfg: withSystem system ({ pkgs, ... }:
        nix-darwin.lib.darwinSystem {
          inherit system;
          specialArgs = { inherit inputs lib pkgs; };
          modules = [
            home-manager.darwinModules.home-manager
            ./modules
            ./darwin/modules
            hostCfg
          ];
        });
    in
    {
      flake = {
        lib = import ./lib { inherit inputs lib; };

        overlays = mapModules ./overlays import;

        nixosConfigurations = mapModules ./nixos/hosts (path: import path {
          inherit nixosSystem;
        });

        darwinConfigurations = mapModules ./darwin/hosts (path: import path {
          inherit darwinSystem;
        });
      };

      systems = [ "x86_64-linux" "aarch64-darwin" ];

      perSystem = { inputs', pkgs, self', system, ... }: {
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
                fenix.overlays.default
                nil.overlays.default
                swayfx.overlays.default
                (_: _: { inherit (inputs'.yonvim.packages) yonvim yonvim-qt; })
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
    });
}

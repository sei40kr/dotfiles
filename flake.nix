{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/25.05";
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    home-manager = {
      url = "github:nix-community/home-manager/release-25.05";
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

    git-hooks = {
      url = "github:cachix/git-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    idea-LazyVim = {
      url = "github:sei40kr/idea-LazyVim";
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

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
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
      git-hooks,
      home-manager,
      nixpkgs,
      nixpkgs-unstable,
      treefmt-nix,
      ...
    }@inputs:
    let
      inherit (flake-parts.lib) mkFlake;
      lib = nixpkgs.lib.extend (lib: _: { my = self.lib; });
      inherit (lib) attrValues;
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
              specialArgs = { inherit inputs inputs' lib; };
              modules = [
                { nixpkgs.pkgs = pkgs; }
                agenix.nixosModules.default
                home-manager.nixosModules.home-manager
                ./modules
                hostCfg
              ];
            }
          );
      in
      {
        imports = [
          git-hooks.flakeModule
          treefmt-nix.flakeModule
        ];

        flake = {
          lib = import ./lib { inherit inputs lib; };

          overlays = import ./overlays;

          nixosConfigurations = import ./hosts { inherit nixosSystem; };
        };

        systems = [ "x86_64-linux" ];

        perSystem =
          {
            config,
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

            config.devShells =
              let
                shells = pkgs.callPackage ./shells { };
              in
              shells
              // {
                default = pkgs.mkShell {
                  shellHook = ''
                    ${config.pre-commit.installationScript}
                  '';
                };
              };

            config.pre-commit.settings.hooks = {
              nil.enable = true;
              statix.enable = true;
              treefmt.enable = true;
            };

            config.treefmt = {
              projectRootFile = "flake.nix";
              programs.nixfmt.enable = true;
            };
          };
      }
    );
}

{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    nixpkgs-unstable.url = "nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    agenix = {
      url = "github:ryantm/agenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url =
      "github:nix-community/emacs-overlay?rev=67fe74d6e73e3c8a983b09a76d809acc730ad911";

    idea-doom-emacs = {
      url = "github:sei40kr/idea-doom-emacs";
      flake = false;
    };
    waybar-scripts = {
      url = "github:sei40kr/waybar-scripts";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, darwin, home-manager
    , emacs-overlay, ... }@inputs:
    let
      inherit (lib)
        attrValues elem filterAttrs genAttrs mkDefault nixosSystem optionalAttrs
        removeSuffix;
      inherit (darwin.lib) darwinSystem;
      inherit (lib.my) mapModules mapModulesRec mapModulesRec';

      lib = nixpkgs.lib.extend
        (lib: _: { my = import ./lib { inherit inputs lib; }; });

      supportedSystems = rec {
        darwin = [ "x86_64-darwin" ];
        linux = [ "x86_64-linux" ];
        all = darwin ++ linux;
      };
      mkPkgs = pkgs: extraOverlays: system:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays ++ (attrValues self.overlays);
        };
      pkgs = genAttrs supportedSystems.all
        (mkPkgs nixpkgs [ emacs-overlay.overlay self.overlay ]);
      pkgs' = genAttrs supportedSystems.all (mkPkgs nixpkgs-unstable [ ]);

      mkNixosHost = { system, ... }@attrs:
        path:
        nixosSystem {
          inherit system;
          specialArgs = {
            inherit lib inputs;
            pkgs = pkgs.${system};
          };
          modules = [
            {
              nixpkgs.pkgs = pkgs.${system};
              networking.hostName =
                mkDefault (removeSuffix ".nix" (baseNameOf path));
            }
            (filterAttrs (n: _: !elem n [ "system" ]) attrs)
            ./modules
            ./nixos-modules
            (import path)
          ];
        };

      mkDarwinHost = path:
        darwinSystem {
          specialArgs = {
            inherit inputs lib;
            pkgs = pkgs.x86_64-darwin;
          };
          modules = [
            {
              networking.hostName =
                mkDefault (removeSuffix ".nix" (baseNameOf path));
            }
            ./modules
            ./darwin-modules
            (import path)
          ];
        };
    in {
      lib = lib.my;

      overlay = _:
        { system, ... }: {
          unstable = pkgs'.${system};
          my = self.packages.${system};
        };

      overlays = mapModules ./overlays import;

      packages = (genAttrs supportedSystems.all (system:
        let
          args = {
            pkgs = pkgs.${system};
            pkgs' = pkgs'.${system};
          };
        in (import ./packages/all args)
        // (optionalAttrs (elem system supportedSystems.darwin)
          (import ./packages/darwin args))
        // (optionalAttrs (elem system supportedSystems.linux)
          (import ./packages/linux args))));

      nixosModules = mapModulesRec ./modules import
        // (mapModulesRec ./nixos-modules import);
      nixosConfigurations =
        mapModules ./hosts/nixos (mkNixosHost { system = "x86_64-linux"; });

      darwinModules = mapModulesRec ./modules import
        // (mapModulesRec ./darwin-modules import);
      darwinConfigurations = mapModules ./hosts/darwin mkDarwinHost;
    };
}

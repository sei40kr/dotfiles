{
  inputs = {
    nixpkgs.url = "nixpkgs/master";
    darwin = {
      url = "github:LnL7/nix-darwin/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    idea-doom-emacs = {
      url = "github:sei40kr/idea-doom-emacs";
      flake = false;
    };
    waybar-scripts = {
      url = "github:sei40kr/waybar-scripts";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, darwin, home-manager, ... }@inputs:
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
      pkgs = genAttrs supportedSystems.all (system:
        import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [ self.overlay ] ++ (attrValues self.overlays);
        });

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

      overlay = _: { system, ... }: { my = self.packages.${system}; };

      overlays = mapModules ./overlays import;

      packages = genAttrs supportedSystems.all
        (system: import ./packages { pkgs = pkgs.${system}; });

      nixosModules = mapModulesRec ./modules import
        // (mapModulesRec ./nixos-modules import);
      nixosConfigurations =
        mapModules ./hosts/nixos (mkNixosHost { system = "x86_64-linux"; });

      darwinModules = mapModulesRec ./modules import
        // (mapModulesRec ./darwin-modules import);
      darwinConfigurations = mapModules ./hosts/darwin mkDarwinHost;
    };
}

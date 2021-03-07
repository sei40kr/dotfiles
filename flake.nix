{
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-20.09";
    nixpkgs-unstable.url = "nixpkgs/master";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    emacs-overlay.url =
      "github:nix-community/emacs-overlay?rev=e18aa3af84a181f86adf7f9cb31d004edcf7d287";
  };

  outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, ... }@inputs:
    let
      inherit (lib) attrValues;
      inherit (lib.my) mapModules mapModulesRec mapHosts;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays ++ (attrValues self.overlays);
        };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [ ];

      lib = nixpkgs.lib.extend
        (lib: _: { my = import ./lib { inherit lib pkgs inputs; }; });
    in {
      lib = lib.my;

      overlay = _: _: {
        unstable = uPkgs;
        my = self.packages."${system}";
      };

      overlays = mapModules ./overlays import;

      packages."${system}" = (import ./packages) { inherit pkgs uPkgs; };

      nixosModules = {
        dotfiles = import ./.;
      } // (mapModulesRec ./modules import)
        // (mapModulesRec ./modules-linux import);

      nixosConfigurations = mapHosts ./hosts { inherit system; };
    };
}

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
    secrets = {
      url = "github:sei40kr/dotfiles-secrets";
      flake = false;
    };

    idea-doom-emacs = {
      url = "github:sei40kr/idea-doom-emacs";
      flake = false;
    };

    yonvim = {
      url = "github:sei40kr/yonvim";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, darwin, ... }@inputs:
    let
      inherit (lib) attrValues elem filterAttrs genAttrs hasSuffix mkDefault
        nixosSystem optionalAttrs removeSuffix;
      inherit (darwin.lib) darwinSystem;
      inherit (lib.my) mapModules mapModulesRec mapModulesRec';

      lib = nixpkgs.lib.extend (lib: _: {
        my = import ./lib { inherit inputs lib; };
      });

      systems = [ "x86_64-darwin" "x86_64-linux" ];
      pkgs' = genAttrs systems (system: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [ self.overlay ] ++ (attrValues self.overlays);
      });

      isLinux = hasSuffix "-linux";
      isDarwin = hasSuffix "-darwin";
      mkHost = path:
        let
          hostCfg = (import path) { inherit inputs lib pkgs'; };
          inherit (hostCfg) system;

          specialArgs = {
            inherit inputs lib;
            pkgs = pkgs'.${system};
          };
          modules = [
            { networking.hostName = mkDefault (removeSuffix ".nix" (baseNameOf path)); }
            ./modules
            (filterAttrs (n: _: !elem n [ "system" "stateVersion" ]) hostCfg)
          ];
        in
        if isLinux system then
          (nixosSystem {
            inherit system specialArgs;
            modules = modules ++ [
              {
                system = { inherit (hostCfg) stateVersion; };
                home-manager.users.${hostCfg.user.name}.home = {
                  inherit (hostCfg) stateVersion;
                };
              }
              ./nixos/modules
            ];
          })
        else if isDarwin system then
          (darwinSystem {
            inherit specialArgs;
            modules = modules ++ [
              {
                home-manager.users.${hostCfg.user.name}.home = {
                  inherit (hostCfg) stateVersion;
                };
              }
              ./darwin/modules
            ];
          })
        else abort "[mkHost] Unknown system architecture: ${system}";
    in
    {
      lib = lib.my;

      overlay = _: { system, ... }: { my = self.packages.${system}; };

      overlays = mapModules ./overlays import;

      packages = genAttrs systems (system: import ./packages {
        pkgs = pkgs'.${system};
      });

      nixosModules = mapModulesRec ./modules import
        // (mapModulesRec ./nixos/modules import);
      nixosConfigurations = mapModules ./nixos/hosts mkHost;

      darwinModules = mapModulesRec ./modules import
        // (mapModulesRec ./darwin/modules import);
      darwinConfigurations = mapModules ./darwin/hosts mkHost;
    };
}

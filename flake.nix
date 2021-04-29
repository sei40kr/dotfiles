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
    emacs-overlay.url =
      "github:nix-community/emacs-overlay?rev=e18aa3af84a181f86adf7f9cb31d004edcf7d287";
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

      supportedSystem = rec {
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
      pkgs = genAttrs supportedSystem.all
        (mkPkgs nixpkgs [ emacs-overlay.overlay self.overlay ]);
      pkgs' = genAttrs supportedSystem.all (mkPkgs nixpkgs-unstable [ ]);

      mkNixosHost = path:
        { system, ... }@attrs:
        nixosSystem {
          inherit system;
          specialArgs = { inherit lib inputs; };
          modules = [
            {
              nixpkgs.pkgs = pkgs.${system};
              networking.hostName =
                mkDefault (removeSuffix ".nix" (baseNameOf path));
            }
            (filterAttrs (n: _: !elem n [ "system" ]) attrs)
            ../.
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
            ({ config, pkgs, ... }: {
              imports = [ inputs.home-manager.darwinModules.home-manager ]
                ++ (mapModulesRec' (toString ./modules/common) import)
                ++ (mapModulesRec' (toString ./modules/darwin) import);

              nix = {
                extraOptions = "experimental-features = nix-command flakes";
                package = pkgs.nixFlakes;
                useDaemon = true;
              };
              users.nix.configureBuildUsers = true;

              system = rec {
                build.applications = pkgs.buildEnv {
                  name = "user-applications";
                  paths = config.users.users.${config.user.name}.packages;
                  pathsToLink = "/Applications";
                };

                activationScripts.applications.text = ''
                  # Set up applications.
                  echo "setting up ~/Applications..." >&2

                  if [ ! -e ~/Applications -o -L ~/Applications ]; then
                    ln -sfn ${build.applications}/Applications ~/Applications
                  else
                    echo "warning: ~/Applications is a directory, skipping App linking..." >&2
                  fi
                '';
              };

              user.home = "/Users/${config.user.name}";
            })
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

      packages = (genAttrs supportedSystem.all (system:
        let
          args = {
            pkgs = pkgs.${system};
            pkgs' = pkgs'.${system};
          };
        in (import ./packages/all args)
        // (optionalAttrs (elem system supportedSystem.darwin)
          (import ./packages/darwin args))
        // (optionalAttrs (elem system supportedSystem.linux)
          (import ./packages/linux args))));

      nixosModules = {
        dotfiles = import ./.;
      } // (mapModulesRec ./modules/common import)
        // (mapModulesRec ./modules/nixos import);
      nixosConfigurations = mapModulesRec ./hosts/nixos mkNixosHost;

      darwinModules = {
        dotfiles = import ./.;
      } // (mapModulesRec ./modules/common import)
        // (mapModulesRec ./modules/darwin import);
      darwinConfigurations = mapModulesRec ./hosts/darwin mkDarwinHost;
    };
}

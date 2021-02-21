{ inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  mkHost = path:
    attrs@{ system ? "x86_64-linux", ... }:
    nixosSystem {
      inherit system;
      specialArgs = { inherit lib inputs; };
      modules = [
        {
          nixpkgs.pkgs = pkgs;
          networking.hostName =
            mkDefault (removeSuffix ".nix" (baseNameOf path));
        }
        (filterAttrs (n: _: !elem n [ "system" ]) attrs)
        ../.
        (import path)
      ];
    };

  mapHosts = dir: attrs: mapModules dir (hostPath: mkHost hostPath attrs);
}

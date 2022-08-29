{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = mapModulesRec' (toString ./.) import;

  environment.variables = {
    DOTFILES = config.dotfiles.dir;
    DOTFILES_BIN = config.dotfiles.binDir;
  };

  nix =
    let
      filteredInputs = filterAttrs (n: _: n != "self") inputs;
      nixPathInputs = mapAttrsToList (n: v: "${n}=${v}") filteredInputs;
      registryInputs = mapAttrs (_: v: { flake = v; }) filteredInputs;
    in
    {
      package = pkgs.nixFlakes;
      extraOptions = "experimental-features = nix-command flakes";
      nixPath = nixPathInputs ++ [
        "nixpkgs-overlays=${config.dotfiles.dir}/overlays"
        "dotfiles=${config.dotfiles.dir}"
      ];
      registry = registryInputs // { dotfiles.flake = inputs.self; };
    };

  environment.systemPackages = with pkgs; [ coreutils git gnumake vim ];
}

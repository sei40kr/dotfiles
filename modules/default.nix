{ config, inputs, lib, pkgs, ... }:

with lib;
with lib.my;
let users = [ "root" config.user.name ];
in
{
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
      nixPath = nixPathInputs ++ [
        "nixpkgs-overlays=${config.dotfiles.dir}/overlays"
        "dotfiles=${config.dotfiles.dir}"
      ];
      extraOptions = "experimental-features = nix-command flakes";
      registry = registryInputs // { dotfiles.flake = inputs.self; };
      settings = {
        allowed-users = users;
        trusted-users = users;
      };
    };

  environment.systemPackages = with pkgs; [ coreutils git gnumake vim ];
}

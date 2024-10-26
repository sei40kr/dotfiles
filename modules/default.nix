{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

with lib;
with lib.my;
let
  users = [
    "root"
    config.user.name
  ];
in
{
  imports = mapModulesRec' (toString ./.) import;

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
      registry = registryInputs // {
        dotfiles.flake = inputs.self;
      };
      settings = {
        allowed-users = users;
        trusted-users = users;
      };
    };

  environment.systemPackages = with pkgs; [
    coreutils
    git
    gnumake
    vim
  ];

  environment.variables = {
    NIXPKGS_ALLOW_UNFREE = "1";
    DOTFILES = config.dotfiles.dir;
    DOTFILES_BIN = config.dotfiles.binDir;
  };

  home-manager.users.${config.user.name}.home.stateVersion = config.system.stateVersion;
}

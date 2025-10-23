{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:

let
  inherit (lib) filterAttrs mapAttrs mapAttrsToList;

  users = [
    "root"
    config.user.name
  ];
in
{
  imports = [
    ./agenix.nix
    ./docker.nix
    ./options.nix
    ./xdg.nix
    ./ai
    ./desktop
    ./dev
    ./editors
    ./i18n
    ./services
    ./shell
    ./term
  ];

  nix.package = pkgs.lix;
  nix.extraOptions = "experimental-features = nix-command flakes";
  nix.settings = {
    allowed-users = users;
    trusted-users = users;
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

  programs.nh = {
    enable = true;
    flake = "/etc/dotfiles";
  };
}

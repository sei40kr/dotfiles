{ config, lib, options, pkgs, ... }:

with lib; {
  imports = [
    ./aws-shell.nix
    ./circleci-cli.nix
    ./docker-compose.nix
    ./git.nix
    ./google-cloud-sdk.nix
    ./gradle.nix
    ./jenv.nix
    ./maven.nix
    ./travis.nix
    ./zeal.nix
  ];
}

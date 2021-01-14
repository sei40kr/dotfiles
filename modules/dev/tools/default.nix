{ lib, ... }:

with lib; {
  imports = [
    ./ansible.nix
    ./aws-cli.nix
    ./aws-shell.nix
    ./circleci-cli.nix
    ./docker-compose.nix
    ./git.nix
    ./google-cloud-sdk.nix
    ./gradle.nix
    ./jenv.nix
    ./jq.nix
    ./jupyter.nix
    ./maven.nix
    ./mycli.nix
    ./pgcli.nix
    ./spring-boot.nix
    ./streamlit.nix
    ./terraform.nix
    ./travis.nix
    ./zeal.nix
  ];
}

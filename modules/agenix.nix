{ config, pkgs, ... }:

{
  config = {
    user.packages = with pkgs; [ agenix ];

    age.identityPaths = [
      "${config.user.home}/.ssh/id_rsa"
      "${config.user.home}/.ssh/id_ed25519"
      "/etc/ssh/ssh_host_rsa_key"
      "/etc/ssh/ssh_host_ed25519_key"
    ];
  };
}

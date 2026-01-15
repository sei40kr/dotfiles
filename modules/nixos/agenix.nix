{
  config,
  inputs,
  perSystem,
  ...
}:

{
  imports = [ inputs.agenix.nixosModules.default ];

  environment.systemPackages = [ perSystem.agenix.default ];

  age.identityPaths = [
    "${config.users.users.sei40kr.home}/.ssh/id_rsa"
    "${config.users.users.sei40kr.home}/.ssh/id_ed25519"
    "/etc/ssh/ssh_host_rsa_key"
    "/etc/ssh/ssh_host_ed25519_key"
  ];
}

{
  config,
  inputs,
  ...
}:

{
  imports = [ inputs.agenix.homeManagerModules.default ];

  config = {
    age.identityPaths = [ "${config.home.homeDirectory}/.ssh/id_ed25519" ];
  };
}

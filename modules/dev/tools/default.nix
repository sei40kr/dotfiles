{ ... }:

{
  imports = [
    ./ansible.nix
    ./aws.nix
    ./azure.nix
    ./github.nix
    ./gitu.nix
    ./jupyter
    ./k8s.nix
    ./mutagen.nix
    ./terraform.nix
  ];
}

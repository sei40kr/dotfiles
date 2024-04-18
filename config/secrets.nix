let
  publicKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFSeAy8gRZOOnY29TLZwDi32v9FdqXCgKLnJr9cG3nQb sei40kr@thinkpad"
  ];
in
{
  "wireguard/wg0/private.age".publicKeys = publicKeys;
  "wireguard/wg0/peer1-psk.age".publicKeys = publicKeys;
}

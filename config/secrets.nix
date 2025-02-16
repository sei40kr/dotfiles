let
  publicKeys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFSeAy8gRZOOnY29TLZwDi32v9FdqXCgKLnJr9cG3nQb sei40kr@thinkpad"
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILg9y0SQ5zWR9G0ZKv5uNlXyTHwnbV7vrODN4F5VwXBV sei40kr@torrent"
  ];
in
{
  "polybar/openweathermap.key.age".publicKeys = publicKeys;
  "polybar/google_calendar_service_account.json.age".publicKeys = publicKeys;
  "wireguard/wg0/private.age".publicKeys = publicKeys;
  "wireguard/wg0/peer1-psk.age".publicKeys = publicKeys;
  "wireguard/wg0/peer2-psk.age".publicKeys = publicKeys;
}

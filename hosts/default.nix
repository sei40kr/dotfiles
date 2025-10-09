{ nixosSystem }:
{
  torrent = import ./torrent { inherit nixosSystem; };
}

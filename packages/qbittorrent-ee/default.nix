{ lib, fetchFromGitHub, qbittorrent }:

let
  version = "4.5.4";
  eeVersion = "4.5.4.10";
in
qbittorrent.overrideAttrs (attrs:
if attrs.version != version then
  throw "Unsupported qBittorrent version: ${attrs.version}"
else {
  pname = "qbittorrent-ee";
  version = eeVersion;

  src = fetchFromGitHub {
    owner = "c0re100";
    repo = "qBittorrent-Enhanced-Edition";
    rev = "release-${eeVersion}";
    hash = "sha256-2kHjW3CMcAEITbYzw8QNRqkTWrMkaktkdAd69moyk2M=";
  };

  meta = with lib; {
    description = "qBittorrent Enhanced, based on qBittorrent";
    homepage = "https://github.com/c0re100/qBittorrent-Enhanced-Edition";
    changelog = "https://github.com/c0re100/qBittorrent-Enhanced-Edition/blob/release-${eeVersion}/Changelog";
    platforms = platforms.linux;
  };
})

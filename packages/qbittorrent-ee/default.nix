{ lib, fetchFromGitHub, qbittorrent }:

qbittorrent.overrideAttrs (attrs:
  if attrs.version != "4.4.5" then
    throw "Unsupported qBittorrent version: ${attrs.version}"
  else {
    pname = "qbittorrent-ee";
    version = "4.4.5.10";

    src = fetchFromGitHub {
      owner = "c0re100";
      repo = "qBittorrent-Enhanced-Edition";
      rev = "release-4.4.5.10";
      sha256 = "sha256-3kJEMLUThABAkDBbErrwEKz9bAg/Gd9/ZsWzmFmLLFg=";
    };

    meta = with lib; {
      description = "qBittorrent Enhanced, based on qBittorrent";
      homepage = "https://github.com/c0re100/qBittorrent-Enhanced-Edition";
      changelog = "https://github.com/c0re100/qBittorrent-Enhanced-Edition/blob/release-4.4.5.10/Changelog";
      license = licenses.gpl2Plus;
      platforms = platforms.linux;
    };
  })

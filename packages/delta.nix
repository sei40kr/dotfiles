{ fetchurl, stdenv, ... }:

let
  version = "0.4.3";
  urlBase = "https://github.com/dandavison/delta/releases/download/${version}/";
  sha256 = if stdenv.hostPlatform.isLinux then
    "1ipxa7sw7pyhxxhvw1ricv0fdzd1ch482ml7md458zgd86j0ivlb"
  else if stdenv.hostPlatform.isDarwin then
    "1in2izd4blq7rjndhhc33g4wk8d3zhw901svb8479k4gciiiai66"
  else
    throw "Unsupported system: ${stdenv.hostPlatform.system}";
  urlStr = if stdenv.hostPlatform.isLinux then
    urlBase + "delta-${version}-x86_64-unknown-linux-gnu.tar.gz"
  else if stdenv.hostPlatform.isDarwin then
    urlBase + "delta-${version}-x86_64-apple-darwin.tar.gz"
  else
    throw "Unsupported system: ${stdenv.hostPlatform.system}";
in stdenv.mkDerivation {
  inherit version;

  pname = "delta";

  src = fetchurl {
    inherit sha256;

    url = urlStr;
  };

  installPhase = ''
    mkdir -p $out/bin
    mv delta "''${out}/bin/delta"
  '';

  meta = with stdenv.lib; {
    description = "A viewer for git and diff output";
    license = licenses.mit;
    platforms = platforms.all;
  };
}

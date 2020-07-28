{ fetchurl, gzip, stdenv, ... }:

let
  version = "0.2.1";
  urlBase =
    "https://github.com/haskell/haskell-language-server/releases/download/${version}/";
  hls_8101 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "1swbdhc4gwgkm2zd41m0c58i53isdysyxc8c6vym9dngv2qd8p1s"
    else if stdenv.hostPlatform.isDarwin then
      "02p99y6kaw6gc9smadj2xlf6xg7mv5wb0irmk4yj4j05s60j8nz5"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.10.1.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.10.1.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls_884 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "04p4qhbspq0j1v6vilyl54r8vms3i2blh4mb54mrdi8xymac1xa4"
    else if stdenv.hostPlatform.isDarwin then
      "1psbwrqai7bziwg8365j040lcb76gkl9gb746mw4d1zr5r5s85n8"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.8.4.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.8.4.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls_883 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "01xkrvq8zny5sfcgp9hn3z9805kbmff918iaf5xibmwx11z0nl95"
    else if stdenv.hostPlatform.isDarwin then
      "02459wqdh30w8cdwhih7kgkppa112ahsnxi4gc52cg7dxvlvmwwm"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.8.3.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.8.3.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls_882 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "1mrr1yfaxxbgap75jvlhjp2rcrpvnba30ldz2yn6kwz10i8yg4rj"
    else if stdenv.hostPlatform.isDarwin then
      "10bry3491dmdw72s84672s0faf1wh9mrr0m6s1a45rcfv03x566g"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.8.2.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.8.2.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls_865 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "08dp4bvh3sf2kj0hcfl37yisf23g0qfkpqwq0v62yk8xpn3ylx1d"
    else if stdenv.hostPlatform.isDarwin then
      "1pv79whn84ykix5z5s448c06jv0ihfr48jjwrdyrbcfs5p0807b6"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.6.5.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.6.5.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls_864 = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "0xz1cs5lalvfny056iwp9lgsdj0fzrxcaq2k6ypliq0xpsnjqk3w"
    else if stdenv.hostPlatform.isDarwin then
      "0zap7p67l9x76q1xnga9kjcghi3b70lld03s0mf8qfkznfn7zlpj"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-Linux-8.6.4.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-macOS-8.6.4.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
  hls-wrapper = let
    sha256 = if stdenv.hostPlatform.isLinux then
      "0ghghx9ywv1xp4n680qvs7wf700cf0bb9vc7dbpj0nn65kx2v0w3"
    else if stdenv.hostPlatform.isDarwin then
      "0v60ii7i8jwm8z2gz7gwwsl451vgnkywc5kis73rzfsdns4zz7rr"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
    urlStr = if stdenv.hostPlatform.isLinux then
      urlBase + "haskell-language-server-wrapper-Linux.gz"
    else if stdenv.hostPlatform.isDarwin then
      urlBase + "haskell-language-server-wrapper-macOS.gz"
    else
      throw "Unsupported system: ${stdenv.hostPlatform.system}";
  in fetchurl {
    inherit sha256;
    url = urlStr;
  };
in stdenv.mkDerivation {
  inherit version;
  pname = "haskell-language-server";

  nativeBuildInputs = [ gzip ];

  buildCommand = ''
    mkdir -p $out/bin
    gunzip -c ${hls_8101} >$out/bin/haskell-language-server-8.10.1
    chmod +x $out/bin/haskell-language-server-8.10.1
    gunzip -c ${hls_884} >$out/bin/haskell-language-server-8.8.4
    chmod +x $out/bin/haskell-language-server-8.8.4
    gunzip -c ${hls_883} >$out/bin/haskell-language-server-8.8.3
    chmod +x $out/bin/haskell-language-server-8.8.3
    gunzip -c ${hls_882} >$out/bin/haskell-language-server-8.8.2
    chmod +x $out/bin/haskell-language-server-8.8.2
    gunzip -c ${hls_865} >$out/bin/haskell-language-server-8.6.5
    chmod +x $out/bin/haskell-language-server-8.6.5
    gunzip -c ${hls_864} >$out/bin/haskell-language-server-8.6.4
    chmod +x $out/bin/haskell-language-server-8.6.4
    gunzip -c ${hls-wrapper} >$out/bin/haskell-language-server-wrapper
    chmod +x $out/bin/haskell-language-server-wrapper
  '';

  meta = with stdenv.lib; {
    description =
      "Integration point for ghcide and haskell-ide-engine. One IDE to rule them all.";
    license = licenses.asl20;
    platforms = platforms.all;
  };
}

{ coreutils, e2fsprogs, fetchurl, git, gnugrep, jdk, lib, libnotify, libsecret
, makeDesktopItem, makeWrapper, patchelf, stdenv, unzip, which, writeText
, vmopts ? null }:

with lib;
let
  name = "data-spell-${version}";
  version = "2021.3";
  vmoptsName = "dataspell"
    + (if (with stdenv.hostPlatform; (is32bit || isDarwin)) then "" else "64")
    + ".vmoptions";
  vmoptsFile =
    lib.optionalString (vmopts != null) (writeText vmoptsName vmopts);
  description = "The IDE for Professional Data Scientists";
  longDescription = ''
    JetBrains DataSpell is an IDE for data science with intelligent Jupyter
    notebooks, interactive Python scripts, and lots of other built-in tools.
  '';
  desktopItem = makeDesktopItem {
    name = "dataspell";
    exec = "dataspell";
    comment = lib.replaceChars [ "\n" ] [ " " ] longDescription;
    desktopName = "DataSpell";
    genericName = description;
    categories = "Development;";
    icon = "dataspell";
    extraEntries = ''
      StartupWMClass=jetbrains-dataspell
    '';
  };
in stdenv.mkDerivation {
  inherit name version desktopItem;

  src = fetchurl {
    url = "https://download.jetbrains.com/python/dataspell-${version}.tar.gz";
    sha256 = "1164rdzzpv4rlvfpgc1srr9qaj1lgmd3pznygxacs8namcd3s1rm";
  };

  postPatch = lib.optionalString (!stdenv.isDarwin) ''
    get_file_size() {
      local fname="$1"
      echo $(ls -l $fname | cut -d ' ' -f5)
    }

    munge_size_hack() {
      local fname="$1"
      local size="$2"
      strip $fname
      truncate --size=$size $fname
    }

    interpreter=$(echo ${stdenv.glibc.out}/lib/ld-linux*.so.2)
    if [[ "${stdenv.hostPlatform.system}" == "x86_64-linux" && -e bin/fsnotifier64 ]]; then
      target_size=$(get_file_size bin/fsnotifier64)
      patchelf --set-interpreter "$interpreter" bin/fsnotifier64
      munge_size_hack bin/fsnotifier64 $target_size
    else
      target_size=$(get_file_size bin/fsnotifier)
      patchelf --set-interpreter "$interpreter" bin/fsnotifier
      munge_size_hack bin/fsnotifier $target_size
    fi
  '';

  nativeBuildInputs = [ makeWrapper patchelf unzip ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/{bin,$name,share/pixmaps,libexec/${name}}
    cp -a . $out/$name
    ln -s $out/$name/bin/dataspell.png $out/share/pixmaps/dataspell.png
    mv bin/fsnotifier* $out/libexec/${name}/.

    jdk=${jdk.home}
    item=${desktopItem}

    makeWrapper "$out/$name/bin/dataspell.sh" "$out/bin/dataspell" \
      --prefix PATH : "$out/libexec/${name}:${
        lib.optionalString (stdenv.isDarwin) "${jdk}/jdk/Contents/Home/bin:"
      }${lib.makeBinPath [ jdk coreutils gnugrep which git ]}" \
      --prefix LD_LIBRARY_PATH : "${
        lib.makeLibraryPath [
          # Some internals want libstdc++.so.6
          stdenv.cc.cc.lib
          libsecret
          e2fsprogs
          libnotify
        ]
      }" \
      --set JDK_HOME "$jdk" \
      --set DATASPELL_JDK "$jdk" \
      --set JAVA_HOME "$jdk" \
      --set DATA_SPELL_VM_OPTIONS ${vmoptsFile}

    ln -s "$item/share/applications" $out/share

    runHook postInstall
  '';

  meta = with lib; {
    inherit description longDescription;
    homepage = "https://www.jetbrains.com/dataspell/";
    license = licenses.unfree;
    platforms = platforms.linux;
    mainProgram = "dataspell";
  };
}

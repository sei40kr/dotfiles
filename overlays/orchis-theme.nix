(_self: super: {
  orchis-theme = super.orchis-theme.overrideAttrs (_: rec {
    version = "2021-10-16";

    src = super.fetchFromGitHub {
      repo = "Orchis-theme";
      owner = "vinceliuice";
      rev = version;
      sha256 = "0r57kzrl9mv26lm1mm6xf8p19c9lx4jxyzncph1iyz0ivaji4ih7";
    };

    postInstall = ''
      mkdir -p $out/share/backgrounds
      mv src/wallpaper/4k.jpg $out/share/backgrounds/orchis-wallpaper.jpg
    '';
  });
})

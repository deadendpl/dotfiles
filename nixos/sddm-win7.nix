{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "sddm-theme";
  src = pkgs.fetchFromGitHub {
    owner = "AudacityXD62";
    repo = "win7welcomesddm";
    rev = "4887e163786657892eee452fddce36e5f02f4780";
    sha256 = "18x4mik24bahqn1ivqzc0120xl12ynd17vh1f8vvq5zlvqn3v78d";
  };
  installPhase = ''
    mkdir -p $out
    cp -R ./* $out/
  '';
}

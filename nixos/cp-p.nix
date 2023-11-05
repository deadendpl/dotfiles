{ pkgs }:

pkgs.stdenv.mkDerivation {
  name = "cp-p";
  src = pkgs.fetchFromGitHub {
    owner = "deadendpl";
    repo = "nix-cp-p";
    rev = "414f7983dfd3abe8048363b2ca1019472802c5b5";
    sha256 = "1vfjx3xh58ihll1sz3v3dmwjag9s0j50gxdzpql6frl1ilrdpwli";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp cp-p mv-p $out/bin/
  '';
}

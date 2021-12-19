{ pkgs ? import <nixpkgs> {} }:

let src = pkgs.fetchFromGitLab {
    owner = "bztsrc";
    repo = "bootboot";
    rev = "6d4df93453b4a5ce024ee98052e23af349039ac4";
    sha256 = "uvjILhEBwY4v7clZWWzuRkPE0IHPwSA1Ch0wbPtkzA0=";
  };

in

{
  mkbootimg = pkgs.stdenv.mkDerivation {
    pname = "mkbootimg";
    version = "git";

    inherit src;


    nativeBuildInputs = with pkgs; [ zip ];
    buildInputs = with pkgs; [ zlib ];

    buildPhase = ''
      cd mkbootimg;
      make
    '';

    installPhase = ''
      mkdir -p $out/bin
      install -m 755 mkbootimg $out/bin
    '';
  };

  include = pkgs.stdenv.mkDerivation {
    pname = "bootboot-include";
    version = "git";

    inherit src;

    buildPhase = "";
    installPhase = ''
      mkdir -p $out/include;
      install -m 644 dist/bootboot.h $out/include
    '';
  };
}

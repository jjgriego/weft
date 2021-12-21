let
  pkgs = import <nixpkgs> {};
  crossPkgs = import <nixpkgs> {
    crossSystem = {
      config = "x86_64-elf";
    };
  };

  bootboot = pkgs.callPackage ./build/bootboot.nix {};
  builder = pkgs.callPackage ./build {};
in

crossPkgs.mkShell {
  name = "weft-dev";
  depsBuildBuild = with pkgs; [ bootboot.mkbootimg (haskellPackages.ghcWithPackages (hs: with hs; [cabal-install]))];
  buildInputs = [ bootboot.include ];

  shellHook = ''
    function weft-build () {
      (
        cd "$(git rev-parse --show-toplevel)"/build;
        cabal run weft-build -- "$@"
      )
    }
  '';
}

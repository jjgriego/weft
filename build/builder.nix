{ mkDerivation, base, lib, shake }:
mkDerivation {
  pname = "weft-build";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base shake ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}

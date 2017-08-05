{ mkDerivation, base, stdenv, unliftio, unliftio-core }:
mkDerivation {
  pname = "unliftio-concurrent";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base unliftio unliftio-core ];
  homepage = "https://github.com/pbogdan/unliftio-concurrent";
  license = stdenv.lib.licenses.bsd3;
}

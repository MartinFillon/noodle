{
  mkDerivation,
  base,
  hspec,
  lib,
  megaparsec,
  mtl,
  scientific,
}:
mkDerivation {
  pname = "noodle";
  version = "0.1.0";
  src = ./.;

  libraryHaskellDepends = [
    base
    megaparsec
    mtl
    scientific
  ];
  testHaskellDepends = [
    base
    hspec
  ];
  homepage = "https://github.com/MartinFillon/noodle#readme";
  description = "Noodle is a Haskell library for parsing and serializing data. It provides a simple and efficient way to work with these popular data formats in Haskell applications.";
  license = lib.meta.getLicenseFromSpdxId "MIT";
}

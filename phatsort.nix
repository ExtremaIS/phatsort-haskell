{ mkDerivation, ansi-wl-pprint, base, directory, filepath
, optparse-applicative, random-shuffle, stdenv, transformers
}:
mkDerivation {
  pname = "phatsort";
  version = "0.1.0.3";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-wl-pprint base directory filepath optparse-applicative
    random-shuffle transformers
  ];
  testHaskellDepends = [ base ];
  homepage = "https://github.com/ExtremaIS/phatsort-haskell#readme";
  description = "FAT filesystem sort utility";
  license = stdenv.lib.licenses.mit;
}

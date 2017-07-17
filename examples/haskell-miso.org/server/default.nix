{ mkDerivation, aeson, base, containers, lucid, miso
, mtl, network-uri, servant, servant-lucid, servant-server, stdenv
, warp
}:
mkDerivation {
  pname = "haskell-miso";
  version = "0.1.0.0";
  src = ../.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base containers lucid mtl network-uri servant
    servant-lucid servant-server warp miso
  ];
  homepage = "https://haskell-miso.org";
  description = "https://haskell-miso.org";
  license = stdenv.lib.licenses.bsd3;
}

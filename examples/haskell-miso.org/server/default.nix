{ mkDerivation, acid-state, aeson, base, containers, miso, mtl
, network-uri, servant-lucid, servant-server, stdenv, warp
}:
mkDerivation {
  pname = "haskell-miso";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    acid-state aeson base containers miso mtl network-uri servant-lucid
    servant-server warp
  ];
  homepage = "https://haskell-miso.org";
  description = "https://haskell-miso.org";
  license = stdenv.lib.licenses.bsd3;
}

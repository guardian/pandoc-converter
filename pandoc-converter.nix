{ mkDerivation, aeson, base, bytestring, containers, generic-lens
, lens, lib, megaparsec, pandoc, servant-options, servant-server
, text, time, transformers, wai, wai-cors, warp
}:
mkDerivation {
  pname = "PandocConverter";
  version = "1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers generic-lens lens megaparsec
    pandoc servant-options servant-server text time transformers wai
    wai-cors warp
  ];
  license = "unknown";
  mainProgram = "PandocConverter";
}

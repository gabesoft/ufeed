{ mkDerivation, aeson, async, base, blaze-builder, blaze-html
, bytestring, connection, containers, HandsomeSoup, hspec
, http-client, http-client-tls, http-media, http-types, hxt
, iso8601-time, lens, mtl, pretty-show, raw-strings-qq, shakespeare
, stdenv, tagsoup, text, time, transformers, utf8-string, wai
, wai-extra, warp, wreq, xml, xml-conduit
}:
mkDerivation {
  pname = "ufeed";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring connection containers HandsomeSoup
    http-client http-client-tls http-media http-types hxt iso8601-time
    lens mtl raw-strings-qq tagsoup text time transformers wreq xml
    xml-conduit
  ];
  executableHaskellDepends = [
    aeson async base blaze-builder blaze-html bytestring http-client
    http-types pretty-show shakespeare text time utf8-string wai
    wai-extra warp
  ];
  testHaskellDepends = [
    base bytestring containers hspec iso8601-time lens raw-strings-qq
    text time xml-conduit
  ];
  doHaddock = false;
  homepage = "https://github.com/gabesoft/ufeed#readme";
  description = "Initial project template from stack";
  license = stdenv.lib.licenses.bsd3;
}

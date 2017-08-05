{ mkDerivation, base, stdenv, transformers }:
mkDerivation {
  pname = "unliftio-core";
  version = "0.1.0.0";
  sha256 = "0wxv6s91wpfv2f5x17lwm04fvghcfnmzqw7p65117pr1r6yz5fcj";
  libraryHaskellDepends = [ base transformers ];
  homepage = "https://github.com/fpco/monad-unlift/tree/master/unliftio-core#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO";
  license = stdenv.lib.licenses.mit;
}

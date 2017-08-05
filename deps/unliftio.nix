{ mkDerivation, async, base, deepseq, directory, filepath
, monad-logger, resourcet, stdenv, transformers, unix
, unliftio-core
}:
mkDerivation {
  pname = "unliftio";
  version = "0.1.0.0";
  sha256 = "053swazav18rrmlwskrgnw99wn7j7wznaadjmsf8nmzk13qzn18i";
  libraryHaskellDepends = [
    async base deepseq directory filepath monad-logger resourcet
    transformers unix unliftio-core
  ];
  homepage = "https://github.com/fpco/monad-unlift/tree/master/unliftio#readme";
  description = "The MonadUnliftIO typeclass for unlifting monads to IO (batteries included)";
  license = stdenv.lib.licenses.mit;
}

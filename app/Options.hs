{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Options(Options(..), options) where

import           Data.Semigroup      ((<>))
import           Data.String         (fromString)
import qualified Data.Text.Lazy
import           Options.Applicative

type LazyText = Data.Text.Lazy.Text

data Options = Options
  { outdir      :: FilePath
  , namespace   :: LazyText
  , sourceFiles :: [FilePath]
  }

outdirP :: Parser FilePath
outdirP = strOption $  long "outdir"
                    <> short 'o'
                    <> value "."
                    <> metavar "PATH"
                    <> help "The base directory in which to create the output files"

-- Type annotation intentionally omitted as it depends on the version of
-- the optparse-applicative package used.
-- namespaceP' :: Parser String                -- Pre 0.14.0
-- namespaceP' :: IsString a => Parser a       -- Post 0.14.0
namespaceP' = strOption $  long "namespace"
                        <> short 'n'
                        <> value ""
                        <> metavar "NAMESPACE"
                        <> help "The Haskell root namespace for the generated modules"

#if MIN_VERSION_optparse_applicative(0,14,0)
namespaceP :: Parser LazyText
namespaceP = namespaceP'
#else
namespaceP :: Parser LazyText
namespaceP = fromString <$> namespaceP'
#endif

sourceFilesP :: Parser [FilePath]
sourceFilesP = some $ strArgument (metavar "FILES...")

options :: ParserInfo Options
options = info (optionsP <**> helper)
    $  fullDesc
    <> progDesc "Generate Haskell source files from .proto specifications"
    <> header "hsprotoc - protoc for Haskell"
  where
    optionsP = Options <$> outdirP <*> namespaceP <*> sourceFilesP

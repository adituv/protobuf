import Distribution.Simple
import Distribution.Simple.PreProcess

import System.FilePath(takeBaseName)

main = defaultMainWithHooks simpleUserHooks {
           hookedPreProcessors = [ ("proto", ppProto) ]
         }
  where

    -- Dummy implementation: creates a module with a string containing
    -- the module's original filename
    ppProto _bi _lbi =
        PreProcessor 
          { platformIndependent = True
          , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
              do
                let fileContents = testPreprocess inFile
                writeFile outFile (testPreprocess inFile)
          }
      where
        testPreprocess fileName = unlines $
          [ concat ["module ", takeBaseName fileName, " where"]
          , ""
          , "someString :: String"
          , "someString = \"" ++ fileName ++ "\""
          , ""
          ]
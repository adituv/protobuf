import Distribution.Simple
import Distribution.Simple.PreProcess


main = defaultMainWithHooks simpleUserHooks {
           hookedPreProcessors = [ ("proto", ppProto) ]
         }

    -- Dummy implementation: creates a module with a string containing
    -- the module's original filename
    ppProto _bi _lbi =
        PreProcessor 
          { platformIndependent = True
          , runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
              writeFile outFile (testPreprocess inFile)
          }
      where

        testPreprocess fileName = unlines $
          [ concat ["module ", stripExtension fileName, " where"]
          , ""
          , "someString :: String"
          , "someString = " ++ fileName
          , ""
          ]

        stripExtension = takeWhile (/= '.')
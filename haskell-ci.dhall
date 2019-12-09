let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:decfd4579e49d96095b3dfaaf82e31db96028ae7656796480e4764f30d60d790

in  haskellCi.generalCi
                [ haskellCi.checkout
                , haskellCi.haskellEnv haskellCi.matrixEnv
                , haskellCi.cabalDeps
                , haskellCi.cabalBuild
                , haskellCi.BuildStep.Name
                    { name = "Get test data"
                    , run =
                        ''
                        sudo apt install lzip
                        make -j
                        ''
                    }
                , haskellCi.cabalTest
                , haskellCi.cabalDoc
                ]

      ( Some
          { ghc =
              [ haskellCi.GHC.GHC802
              , haskellCi.GHC.GHC822
              , haskellCi.GHC.GHC844
              , haskellCi.GHC.GHC865
              , haskellCi.GHC.GHC881
              ]
          , cabal = [ haskellCi.Cabal.Cabal30 ]
          }
      ) : haskellCi.CI.Type

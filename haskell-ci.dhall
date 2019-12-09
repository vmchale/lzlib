let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:43e43c219449e46c993390815a348d69212e16d53f26e2003fd54857bba53d10

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
                , haskellCi.hlintDirs [ "src", "test", "bench" ]
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

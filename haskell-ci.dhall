let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:710eac6d3194a4590e8b1ec21cde38fa652f0d7e5e805d6ee3e3593ca4012ca4

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
          , operating-system = [ haskellCi.OS.Ubuntu1804 ]
          }
      ) : haskellCi.CI.Type

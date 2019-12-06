let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:ff0522efb1b85daaf578203a42e1caad156d6d461b318c1e7b83c3fcf5d144ba

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

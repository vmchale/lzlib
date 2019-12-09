let haskellCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:bb6b0eee75d9f5d9e62b7a0386efef5c1d0d6fb1415eab5a33500976cc70c886

in    haskellCi.generalCi
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
        )
    : haskellCi.CI.Type

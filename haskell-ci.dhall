let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall

in  haskellCi.defaultWith
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
     (haskellCi.mkMatrix { ghc = [ "8.0.2", "8.2.2", "8.4.4", "8.6.5", "8.8.1" ], cabal = [ "3.0" ] })

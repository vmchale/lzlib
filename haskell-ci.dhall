let VersionInfo = { ghc-version : Text, cabal-version : Text }

let BuildStep =
      < Uses : { uses : Text, with : Optional VersionInfo }
      | Name : { name : Text, run : Text }
      >

let cabalDeps =
      BuildStep.Name
        { name = "Install dependencies"
        , run =
            ''
            cabal update
            cabal build --enable-tests --enable-benchmarks --only-dependencies
            ''
        }

let cabalBuild =
      BuildStep.Name
        { name = "Build"
        , run = "cabal build --enable-tests --enable-benchmarks"
        }

let cabalTest = BuildStep.Name { name = "Tests", run = "cabal test" }

let cabalDoc = BuildStep.Name { name = "Documentation", run = "cabal haddock" }

in  { name = "Haskell CI"
    , on = [ "push" ]
    , jobs =
        { build =
            { runs-on = "ubuntu-latest"
            , steps =
                [ BuildStep.Uses
                    { uses = "actions/checkout@v1", with = None VersionInfo }
                , BuildStep.Uses
                    { uses = "actions/setup-haskell@v1"
                    , with =
                        Some { ghc-version = "8.8.1", cabal-version = "3.0" }
                    }
                , cabalDeps
                , cabalBuild
                , BuildStep.Name
                    { name = "Get test data"
                    , run =
                        ''
                        sudo apt install lzip
                        make -j
                        ''
                    }
                , cabalTest
                , cabalDoc
                ]
            }
        }
    }

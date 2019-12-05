let VersionInfo = { ghc-version : Text, cabal-version : Text }

let BuildStep =
      < Uses : { uses : Text, with : Optional VersionInfo }
      | Name : { name : Text, run : List Text }
      >

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
                , BuildStep.Name
                    { name = "Install dependencies"
                    , run =
                        [ "sudo apt install lzip"
                        , "cabal update"
                        , "cabal build lzlib-test --only-dependencies"
                        , "make -j"
                        ]
                    }
                , BuildStep.Name { name = "Build", run = [ "cabal build" ] }
                , BuildStep.Name { name = "Tests", run = [ "cabal test" ] }
                ]
            }
        }
    }

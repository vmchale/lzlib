let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:6468d335d3d9ae593ca92c3cf08ecbf07571e662272608c3e98ea19ae7c8dd58

in    dhallCi.dhallSteps
        [ dhallCi.dhallYamlInstall
        , dhallCi.checkDhallYaml
            [ "dhall-ci.dhall"
            , "yaml-ci.dhall"
            , "hlint-ci.dhall"
            , "haskell-ci.dhall"
            ]
        ]
    : dhallCi.CI

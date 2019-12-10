let dhallCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/dhall-ci.dhall sha256:8923d42348505bcd1ffe646807ef442fd0c39fad9060df830bf41d25b7918145

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

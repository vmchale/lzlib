let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:710eac6d3194a4590e8b1ec21cde38fa652f0d7e5e805d6ee3e3593ca4012ca4

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type

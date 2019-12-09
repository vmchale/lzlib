let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:decfd4579e49d96095b3dfaaf82e31db96028ae7656796480e4764f30d60d790

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type

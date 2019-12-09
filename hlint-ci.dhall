let haskellCi = https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/haskell-ci.dhall sha256:54219f032460ae0c0300a3ae535747e4a9c89b5ffca74ad73a782fc5142f3d86

in  haskellCi.hlintAction [ "src", "test", "bench" ] : haskellCi.CI.Type

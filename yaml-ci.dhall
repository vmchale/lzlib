let yamlCi =
      https://raw.githubusercontent.com/vmchale/github-actions-dhall/master/yaml-ci.dhall

in  yamlCi.yamlCi [ "stack.yaml" ] : yamlCi.CI

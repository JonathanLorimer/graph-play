{s}: 
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:graph-play' --allow-eval --warnings";
  testScript = s "test" "cabal run test:graph-play-tests";
  hoogleScript = s "hgl" "hoogle serve";
}

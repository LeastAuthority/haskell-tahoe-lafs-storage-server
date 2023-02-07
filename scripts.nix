{s}: rec
{
  ghcidScript = s "dev" "ghcid --command 'cabal new-repl lib:haskell-tahoe-lafs-storage-server' --allow-eval --warnings";
  allScripts = [ghcidScript];
}

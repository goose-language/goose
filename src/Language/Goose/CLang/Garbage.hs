module Language.Goose.CLang.Garbage where

startGarbage :: String -> String
startGarbage mainCode = do
  let init' = "tgc_start(gc(), &argc);"
  let end = "tgc_stop(gc());"
  unlines [init', mainCode, end]

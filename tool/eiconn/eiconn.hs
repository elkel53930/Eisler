import System.Environment

main = do
  arg <- getArgs
  let pn = (read $ head arg)
  putStrLn $ concat [ "defpart CONN"
                    , show pn
                    , "("
                    , pins pn
                    , ")"
                    , "{ref \"CN\";}"
                    ]

pins :: Int -> String
pins x = concat [concat [ show y, ":_", show y, " "] | y <- [1..x] ]

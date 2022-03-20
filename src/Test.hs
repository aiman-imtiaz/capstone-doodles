module Test where

  x :: Maybe Int
  x = Nothing
  
  data StringSeq = Empty                    -- the empty sequence
                 | Cat StringSeq StringSeq  -- two sequences in succession
                 | Single String 
                 
  size ::  [Int] -> [Int]
  size lst =
     case lst ++ lst of
       c@(h1 : h: t) -> c ++ t
       c@(h : t) -> c ++ t
       [] -> []
  
  y :: Int
  y = 10
module Text.Semdoc.Util where

import qualified Data.Char as Char


splitR :: (Char -> Bool) -> String -> [String]
splitR _ [] = []
splitR p s =
  let
    go :: Char -> String -> [String]
    go m s' = case break p s' of
      (b', [])     -> [ m:b' ]
      (b', (x:xs)) -> ( m:b' ) : go x xs
  in case break p s of
    (b,  [])    -> [ b ]
    ([], (h:t)) -> go h t
    (b,  (h:t)) -> b : go h t


splitCamel :: String -> [String]
splitCamel = splitR (\x -> Char.isUpper x || Char.isDigit x)

module Parser (parse) where

import Types

parse :: String -> Program
parse = unlabel . parse'


parse' :: String -> ProgramWithLabels
parse' = undefined


unlabel :: ProgramWithLabels -> Program
unlabel = undefined
import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Lazy.Char8 as BSChar8
import Local.IO.AdventOfCode

today = "12"

jsonSum :: Value -> Integer
jsonSum value = case value of
                  Object obj -> sum $ map (jsonSum . snd) $ HM.toList obj
                  Array arr  -> sum $ V.map jsonSum arr
                  String str -> 0
                  Number num -> truncate num

unred :: Value -> Value
unred value = case value of
                Object obj -> if String (T.pack "red") `elem` HM.elems obj
                              then Number 0 else Object (HM.map unred obj)
                Array arr  -> Array (V.map unred arr)
                any        -> any

main = adventIO today parseContent part1 part2 where
    parseContent = fromJust . decode . BSChar8.pack :: String -> Value
    part1 = jsonSum
    part2 = jsonSum . unred

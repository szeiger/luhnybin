import Data.List
import Data.Char
import Data.Maybe
import GHC.IO.Handle
import GHC.IO.Handle.FD

luhn cs = lsum cs `mod` 10 == 0
  where lsum = sum . zipWith f [1..] . map digitToInt . reverse
        f i x = let y = (2 - mod i 2) * x in if y > 9 then y - 9 else y

luhnStart cs =
  if length ds < 14 then 0
  else fromMaybe 0 $ find (>0) $ map (dluhn ds) [16,15,14]
  where ds = filter isDigit cs
        dluhn cs len = let cs2 = take len cs in
          if length cs2 < len then 0 else if luhn cs2 then len else 0

mapGroups cs = concatMap (tr 0) $ groupBy (\a b -> isD a == isD b) cs
  where isD = flip elem "0123456789- "

tr seq [] = []
tr seq (x:xs) =
  let m = max seq i
      i = luhnStart $ x:xs in
    if isDigit x then (if m > 0 then 'X' else x) : tr (m-1) xs
    else x : tr seq xs

main = hSetBinaryMode stdin True >> hSetBinaryMode stdout True >>
  getContents >>= putStr . mapGroups

import Data.List
import Data.Char
import Data.Maybe
import System.IO

luhn cs = lsum cs `mod` 10 == 0
  where lsum = sum . zipWith f [1..] . map digitToInt . reverse
        f i x = uncurry (+) $ ((2-i `mod` 2) * x) `divMod` 10

luhnStart cs = fromMaybe 0 $ find (>0) $ map (dluhn cs) [16,15,14]
  where dluhn cs len = let es = take len cs in
          if length es >= len && luhn es then len else 0

tr seq [] = []
tr seq (x:xs) =
  let m = max seq $ luhnStart $ filter isDigit $ x:xs in
    if isDigit x then (if m > 0 then 'X' else x) : tr (m-1) xs
    else x : tr seq xs

mapGroups cs = groupBy (\a b -> isD a == isD b) cs >>= tr 0
  where isD c = elem c "0123456789- "

main = mapM (flip hSetBinaryMode True) [stdin, stdout] >>
  getContents >>= putStr . mapGroups

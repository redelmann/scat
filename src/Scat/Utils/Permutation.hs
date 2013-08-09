
{- Copyright (c) 2013 the authors listed at the following URL, and/or
the authors of referenced articles or incorporated external code:
http://en.literateprograms.org/Kth_permutation_(Haskell)?action=history&offset=20090329064426

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Kth_permutation_(Haskell)?oldid=16316
-}

{- | Permutations, taken from the
     http://en.literateprograms.org/Kth_permutation_(Haskell) webpage. -}
module Scat.Utils.Permutation (perm) where

rr :: Int -> Int -> [Int]
rr 0 _ = []
rr n k = k `mod` n : rr (n - 1) (k `div` n)

dfr :: [Int] -> [Int]
dfr = foldr (\ x rs -> x : [r + (if x <= r then 1 else 0) | r <- rs]) []

-- | List permutation.
perm :: [a] -> Int -> [a]
perm xs k = [xs !! i | i <- dfr (rr (length xs) k)]

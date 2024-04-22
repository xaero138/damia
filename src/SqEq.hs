module SqEq (solveSqEq, discriminant) where

-- | solve equation of kind a * x ^ 2 + b * x + c
solveSqEq :: (Ord a, Floating a) => a -> a -> a -> [a]
solveSqEq a b c =
    if d < 0 then []
      else
        eqReduction
          [ (-b +- sqrt d) / (2 * a)
          | (+-) <- [(+), (-)]
          ]
  where
    eqReduction r@[x1,x2] = if x1 == x2 then [x1] else r
    d = discriminant a b c

discriminant :: Num a => a -> a -> a -> a
discriminant a b c = b * b - 4 * a * c

import Control.Monad

-- |fib computes the nth element of the fibonacci sequence initialized by the given pair
fib :: Floating a => (a, a) -> Int -> a
fib u 0 = fst u
fib u 1 = snd u
fib u n = fib u (n-1) + fib u (n-2)

-- |r is the golden number, wich satisfies the property r^2 = r +1
r :: Floating a => a
r = (1 - sqrt 5)/2

-- | fibReq is the sequence of the fibonacci sequence defined by u0 = 1, u1 = (1 - sqrt(5))/2 suite
fibReq :: Floating a => [a]
fibReq = map (fib (1,r)) [0..]

-- |fibs computes the nth element of fibReq
fibs :: Floating a => Int -> a
fibs n = fibReq !! n

-- |fibs' is the direct computation of the nth element of the fibonacci sequence defined in fibReq
fibs' :: Floating a => Int -> a
fibs' n = r^^n

-- |fibSeq prints the sequence of (n, fibs n, fibs' n) for all n > 0
fibSeq :: IO ()
fibSeq = print $ [(n, fibs n, fibs' n) | n <- [1..]]

module Main where

data Complex = Complex Double Double deriving (Show)

data Qubit = Qubit Complex Complex deriving (Show)

hadamard :: Qubit -> Qubit
hadamard (Qubit (Complex a b) (Complex c d)) =
    let factor = 1 / sqrt 2
        newA = Complex ((a + b) * factor) ((a - b) * factor)
        newC = Complex ((c + d) * factor) ((c - d) * factor)
    in Qubit newA newC

cnot :: Qubit -> Qubit -> (Qubit, Qubit)
cnot (Qubit (Complex a b) _) (Qubit (Complex c d) (Complex e f)) =
    let newA = Complex a (if a == 1 then (e - c) else e)
        newB = Complex b (if a == 1 then (f - d) else f)
        newC = Complex c (if a == 1 then (a - e) else a)
        newD = Complex d (if a == 1 then (b - f) else b)
    in (Qubit newA newB, Qubit newC newD)

main :: IO ()
main = do
    let qubit = Qubit (Complex 1 0) (Complex 0 0) 
    let result = hadamard qubit 
    putStrLn $ "Result after applying Hadamard gate: " ++ show result
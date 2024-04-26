module Main where

data Complex = Complex Double Double deriving (Show)

data Qubit = Qubit Complex Complex deriving (Show)

hadamard :: Qubit -> Qubit
hadamard (Qubit (Complex a b) (Complex c d)) =
    let factor = 1 / sqrt 2
        newA = Complex ((a + b) * factor) ((a - b) * factor)
        newC = Complex ((c + d) * factor) ((c - d) * factor)
    in Qubit newA newC

main :: IO ()
main = do
    let qubit = Qubit (Complex 1 0) (Complex 0 0) 
    let result = hadamard qubit 
    putStrLn $ "Result after applying Hadamard gate: " ++ show result
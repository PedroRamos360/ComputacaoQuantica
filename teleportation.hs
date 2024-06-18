-- teleportation.hs

module Main where

import System.Random (randomRIO)
import Data.Complex (Complex((:+)), magnitude, realPart, imagPart)

-- Define Qubit type
type Qubit = (Complex Double, Complex Double)

-- Quantum gates
hadamard :: Qubit -> Qubit
hadamard (a, b) = ((a + b) / sqrt 2, (a - b) / sqrt 2)

pauliX :: Qubit -> Qubit
pauliX (a, b) = (b, a)

pauliY :: Qubit -> Qubit
pauliY (a, b) = (0 :+ (-realPart a), imagPart b :+ 0)

pauliZ :: Qubit -> Qubit
pauliZ (a, b) = (a, -b)

cnot :: Qubit -> Qubit -> (Qubit, Qubit)
cnot (a1, b1) (a2, b2) =
    if magnitude b1 == 0 then ((a1, b1), (a2, b2))
    else ((a1, b1), pauliX (a2, b2))

-- Quantum teleportation protocol
teleportation :: Qubit -> IO Int
teleportation psi = do
    let (aliceQubit, bobQubit) = createEntangledPair
        (aliceQubit', bobQubit') = applyBellMeasurement psi aliceQubit
        bobQubit'' = applyCorrection aliceQubit' bobQubit'
    measure bobQubit''

-- Helper functions
createEntangledPair :: (Qubit, Qubit)
createEntangledPair = 
    let entangledQubit = hadamard (1 :+ 0, 0 :+ 0)
    in (entangledQubit, snd $ cnot entangledQubit (0 :+ 0, 1 :+ 0))

applyBellMeasurement :: Qubit -> Qubit -> (Qubit, Qubit)
applyBellMeasurement psi aliceQubit =
    let (aliceQubit', psi') = cnot psi aliceQubit
        aliceQubit'' = hadamard aliceQubit'
    in (aliceQubit'', psi')

applyCorrection :: Qubit -> Qubit -> Qubit
applyCorrection (a1, b1) bobQubit' =
    let bobQubit'' = if magnitude b1 == 1 then pauliX bobQubit' else bobQubit'
        bobQubit''' = if magnitude a1 == 1 then pauliZ bobQubit'' else bobQubit''
    in bobQubit'''

-- Measurement function
measure :: Qubit -> IO Int
measure (a, b) = do
    let prob0 = magnitudeSquared a
        prob1 = magnitudeSquared b
        totalProb = prob0 + prob1
    r <- randomRIO (0.0, totalProb)
    if r < prob0
        then return 0
        else return 1

magnitudeSquared :: Complex Double -> Double
magnitudeSquared x = magnitude x ** 2

-- Main function
main :: IO ()
main = do
    -- O operador :+ serve para criar números complexos em haskell
    -- A linha de baixo significa 1 + 0i e 0 + 0i
    let psi = (1 :+ 0, 0 :+ 0)  -- Example qubit state (|0⟩)
    result <- teleportation psi
    putStrLn $ "Teleported qubit state: " ++ show result

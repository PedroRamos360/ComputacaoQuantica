module Main where

import System.Random (randomRIO)
import Data.Complex (Complex((:+)), magnitude, realPart, imagPart)

type Qubit = (Complex Double, Complex Double)

hadamard :: Qubit -> Qubit
hadamard (a, b) = ((a + b) / sqrt 2, (a - b) / sqrt 2)

pauliX :: Qubit -> Qubit
pauliX (a, b) = (b, a)

pauliZ :: Qubit -> Qubit
pauliZ (a, b) = (a, -b)

cnot :: Qubit -> Qubit -> (Qubit, Qubit)
cnot (a1, b1) (a2, b2) =
    if magnitude b1 == 0 then ((a1, b1), (a2, b2))
    else ((a1, b1), pauliX (a2, b2))

teleportation :: Qubit -> IO Int
teleportation psi = do
    let (aliceQubit, bobQubit) = createEntangledPair
        (aliceQubit', bobQubit') = applyBellMeasurement psi aliceQubit
        bobQubit'' = applyCorrection aliceQubit' bobQubit'
    measure bobQubit''

createEntangledPair :: (Qubit, Qubit)
createEntangledPair = 
    let entangledQubit = hadamard (1 :+ 0, 0 :+ 0)
    in (entangledQubit, snd $ cnot entangledQubit (0 :+ 0, 1 :+ 0))

applyBellMeasurement :: Qubit -> Qubit -> (Qubit, Qubit)
applyBellMeasurement psi aliceQubit =
    let (psi', aliceQubit') = cnot psi aliceQubit
        aliceQubit'' = hadamard aliceQubit'
    in (aliceQubit'', psi')

applyCorrection :: Qubit -> Qubit -> Qubit
applyCorrection (a1, b1) bobQubit' =
    let bobQubit'' = if magnitude b1 == 1 then pauliX bobQubit' else bobQubit'
        bobQubit''' = if magnitude a1 == 1 then pauliZ bobQubit'' else bobQubit''
    in bobQubit'''

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

main :: IO ()
main = do
    let psi = (1 :+ 0, 0 :+ 0)
    result <- teleportation psi
    putStrLn $ "Teleported qubit state: " ++ show result
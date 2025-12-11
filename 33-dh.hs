{-# LANGUAGE BangPatterns #-}

module Main where

import System.Random (randomRIO)

-- RFC 3526 1536-bit MODP group prime; generator g = 2
primeP :: Integer
primeP = 0xFFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF

generatorG :: Integer
generatorG = 2

-- Fast modular exponentiation by squaring
powMod :: Integer -> Integer -> Integer -> Integer
powMod base exponent modulus = go base exponent 1
  where
    go _ 0 !acc = acc
    go b e !acc
      | even e = go ((b * b) `mod` modulus) (e `div` 2) acc
      | otherwise = go ((b * b) `mod` modulus) (e `div` 2) ((acc * b) `mod` modulus)

data DhParams = DhParams
  { p :: Integer
  , g :: Integer
  }

data KeyPair = KeyPair
  { privateKey :: Integer
  , publicKey :: Integer
  }

-- Generate a private/public key pair for the given parameters
mkKeyPair :: DhParams -> IO KeyPair
mkKeyPair params = do
  priv <- randomRIO (2, p params - 2)
  let pub = powMod (g params) priv (p params)
  pure KeyPair { privateKey = priv, publicKey = pub }

-- Derive the shared secret using our private key and the peer's public key
deriveShared :: DhParams -> Integer -> Integer -> Integer
deriveShared params priv peerPub = powMod peerPub priv (p params)

runDemo :: IO ()
runDemo = do
  let params = DhParams { p = primeP, g = generatorG }
  alice <- mkKeyPair params
  bob <- mkKeyPair params

  let aliceShared = deriveShared params (privateKey alice) (publicKey bob)
      bobShared = deriveShared params (privateKey bob) (publicKey alice)

  putStrLn "=== Diffie-Hellman ==="
  putStrLn $ "Prime p (bits): " ++ show (integerBitSize $ p params)
  putStrLn $ "Generator g: " ++ show (g params)
  putStrLn $ "Alice public A: " ++ show (publicKey alice)
  putStrLn $ "Bob public B:   " ++ show (publicKey bob)
  putStrLn $ "Shared equal?  " ++ show (aliceShared == bobShared)
  putStrLn $ "Shared secret:  " ++ show aliceShared

-- Count significant bits of a positive integer
integerBitSize :: Integer -> Int
integerBitSize 0 = 0
integerBitSize n = go n 0
  where
    go 0 acc = acc
    go x acc = go (x `div` 2) (acc + 1)

main :: IO ()
main = runDemo

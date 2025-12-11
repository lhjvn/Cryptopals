
import Data.Char (digitToInt)
import Data.Bits (testBit)

decodeHex :: String -> [Int]
decodeHex [] = []
decodeHex (x1:x2:xs) = hexToInt [x1, x2] : decodeHex xs
  where
    hexToInt :: String -> Int
    hexToInt [a, b] = (digitToInt a * 16) + digitToInt b
    hexToInt _ = error "Invalid hex string"

encodeBase64 :: [Int] -> String
encodeBase64 = map intToChar . groupBits 6 . concatMap (\n -> intToBits n 8)
    where
        intToBits n bits = map (testBit n) [bits-1, bits-2..0]
        groupBits _ [] = []
        groupBits n xs = let (chunk, rest) = splitAt n xs
                                         in bitsToInt chunk 0 : groupBits n rest
        bitsToInt [] acc = acc
        bitsToInt (b:bs) acc = bitsToInt bs (acc * 2 + if b then 1 else 0)
        intToChar i = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" !! i

main :: IO ()
main = putStrLn $ encodeBase64 $ decodeHex "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

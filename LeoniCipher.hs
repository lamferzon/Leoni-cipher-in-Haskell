import Data.Char
import System.Random
------ Constants
start = 40
end = 127
minLag = 8
maxLag = 25
------ Utils
removeLastChar (x:[]) = []
removeLastChar (x:xs) = x : removeLastChar xs
------ Functions for the modified Caesar cipher
---- Calculation of the lag 
getLag string = div (ord(lastChar) - 2) 5
              where lastChar = head (drop (length string - 1) string)
---- Shifting and shifting checking	  
checkP sum = if sum > end then start + diff - 1 else sum
           where diff = sum - end
--
checkM sum = if sum < start then end - diff + 1 else sum
           where diff = start - sum
--
shiftCaesar char lag mod | mod > 0 = chr (checkP ((ord char) + lag))
                         | mod < 0 = chr (checkM ((ord char) - lag))
---- Crypter and decrypter
decryptsCaesar (x:[]) lag = []
decryptsCaesar (x:xs) lag = [shiftCaesar x lag (-1)] ++ decryptsCaesar xs lag
encryptsCaesar [] lag = [chr(lag*5 + 2)]
encryptsCaesar (x:xs) lag = [shiftCaesar x lag 1] ++ encryptsCaesar xs lag
------ Functions for the Vigenère cipher
---- Key generator
generateKeyIt baseKey len | len > 0 = baseKey ++ generateKeyIt baseKey newLen
                          | len <= 0 = []
                          where newLen = len - length baseKey
--
generateKey s baseKey = take (len) (generateKeyIt baseKey len)
                      where len = length s
---- Shifting and shifting checking
shiftVig char1 char2 mod | mod > 0 = chr (checkP ((ord char1) + (ord char2)))
                         | mod < 0 = chr (checkM ((ord char1) - (ord char2)))
---- Crypter and decrypter
encryptsVig xs ys = [shiftVig (fst i) (snd i) 1 | i <- zip xs ys]
decryptsVig xs ys = [shiftVig (fst i) (snd i) (-1) | i <- zip xs ys]
------ Starting application
encrypts str key lag = encryptsCaesar (encryptsVig (str) (generateKey str key)) lag
--
decrypts str key = decryptsVig (decryptsCaesar str (getLag str)) (generateKey (removeLastChar str) key)
--
repUp s k l num | num > 1 = encrypts (repUp s k l iter) (k) (l)
                | num == 1 = encrypts s k l
                where iter = num - 1
repDown s k num | num > 1 = decrypts (repDown s k iter) (k)
                | num == 1 = decrypts s k 
                where iter = num - 1
--
startApp choice baseKey string lag iter | choice == "1" = repUp string baseKey lag iter
                                        | choice == "2" = repDown string baseKey iter
--
main = do
    lag <- randomRIO (minLag, maxLag :: Int)
    putStrLn "** Leoni cipher **"
    putStrLn "Copyright 2023 Lorenzo Leoni (UniBG)"
    putStrLn " "
    putStrLn "PN: use only fonts from '(' (pos. 40 in ASCII table) to 'HOME' (pos. 127 in ASCII table)"
    putStrLn " "
    putStr "Insert a word or a phrase: "
    string <- getLine
    putStr "Insert a key: "
    myKey <- getLine
    putStr "Insert the number of iterations: "
    input <- getLine
    let iter = read input :: Int
    putStrLn " "
    putStrLn "What do you wanto to do?"
    putStrLn "1. Encrypting"
    putStrLn "2. Decrypting"
    putStr "Your choice: "
    choice <- getLine
    putStrLn " "
    putStr "Result: "
    putStrLn (startApp choice myKey string lag iter)
    putStrLn " "
    putStrLn "Bye bye!"
--
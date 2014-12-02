import Codec.Digest.SHA
import qualified Data.ByteString as B
import System.IO


doManyHashes :: Int -> IO Bool
doManyHashes 0 = do
    return True

doManyHashes counter = do
    contents <- B.readFile "/tmp/data.0512"
    -- let contentsWord8 = B.unpack contents
    let res = hash SHA256 contents
    -- let oct = toOctets hash
    -- putStr "."
    -- hFlush stdout
    -- putStrLn (show $ showBSasHex res)
    doManyHashes $ counter-1


main = do
    doManyHashes 500000
    -- putStrLn ""
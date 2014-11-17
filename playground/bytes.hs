import System.Environment
import System.IO.Error

import qualified Data.ByteString.Lazy   as L
import qualified Data.ByteString        as S


main = do
    print . show . L.cons' 85 . L.pack $ [99, 97, 100]
    print . show . foldr L.cons' L.empty $ [50..60]
    -- now do the copyFile function:
    (fileName1:fileName2:_) <- getArgs
    copyFile fileName1 fileName2


copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = do
    content <- L.readFile src
    L.writeFile dest content

{- TODO
    Consider also outputting metadata and delays between frames
-}

import qualified Data.ByteString as BS
import System.FilePath
import System.Directory
import Control.Monad
import Text.Printf
import System.Environment
import Data.List

import Codec.Picture
import Codec.Picture.Gif

unwrap :: Show err => Either err ok -> ok
unwrap (Left err) = error (show err)
unwrap (Right ok) = ok

main = do
    args <- getArgs
    when (length args == 0) $ error "Usage: splitgif [images]"
    forM_ args $ \gifPath_ -> do
        let gifPath = if ".gif" `isSuffixOf` gifPath_ then gifPath_ else gifPath_ ++ ".gif"
        let outDir = takeBaseName gifPath
        splitGif gifPath outDir

splitGif gifPath outDir = do
    gifContents <- BS.readFile gifPath
    let
        frames = unwrap $ decodeGifImages gifContents
        {-(_, meta) = unwrap $ decodeGifWithMetadata gifContents
        delays = unwrap $ getDelaysGifImages gifContents-}
    printf "Writing frames from %s to %s...\n" gifPath outDir
    createDirectoryIfMissing True outDir
    forM_ (zip frames [0..]) $ \(frame, i) ->
        savePngImage (outDir </> show i ++ ".png") frame
    putStrLn "Done"

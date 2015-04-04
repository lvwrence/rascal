import System.IO
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Bencode
import Tracker

main :: IO ()
main =
    getArgs >>= \args ->
    openFile (head args) ReadMode >>= \handle ->
    hSetEncoding handle latin1 >>
    hGetContents handle >>= \contents ->
    -- send initial request
    requestTracker (fromJust (bParse contents)) >>= \resp ->
    putStrLn resp




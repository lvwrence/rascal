import System.IO
import System.Environment (getArgs)
import Data.Maybe (fromJust)
import Data.Map ((!))
import Data.Char (ord)
import Bencode
import Tracker (requestTracker)
import Network
import Data.Binary.Get (runGet, getWord16be)
import qualified Data.ByteString.Lazy.Char8 as B

data Peer = Peer HostName PortNumber deriving Show

main :: IO ()
main =
    getArgs >>= \args ->
    openFile (head args) ReadMode >>= \handle ->
    hSetEncoding handle latin1 >>
    hGetContents handle >>= \contents ->
    -- send initial request
    requestTracker (fromJust (bParse contents)) >>= \resp ->
    putStrLn (show (getPeers resp))

-- | Takes in the response given by the tracker (is assumed to be successful) and returns
-- the list of peers.
getPeers :: String -> [Peer]
getPeers res =
    let peerString = case (fromJust (bParse res)) of BDict b -> case (b ! "peers") of BString s -> s
    in Prelude.map parsePeer (splitEvery6 (B.pack peerString))

-- | Splits every 6 characters.
splitEvery6 :: B.ByteString -> [B.ByteString]
splitEvery6 s = if (B.empty == s)
             then []
             else (B.take 6 s) : (splitEvery6 (B.drop 6 s))

-- | Parses a ByteString and constructs a Peer from it.
parsePeer :: B.ByteString -> Peer
parsePeer b =
    let parseHostName = init . B.foldl (\cur x -> cur ++ show (ord x) ++ ".") ""
        parsePortNumber = fromIntegral . runGet getWord16be
    in Peer (parseHostName (B.take 4 b)) (parsePortNumber (B.drop 4 b))

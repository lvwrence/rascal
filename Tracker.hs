module Tracker
    (
        requestTracker,
        createTrackerRequest
    ) where

import Bencode
import Network.HTTP
import Data.List.Utils
import Data.Map

import qualified Data.ByteString.Char8 as B
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Base16 as HexEncode

requestTracker :: Bencode -> IO String
requestTracker b = initiateTrackerRequest (createTrackerRequest b)

announceURL :: Bencode -> String
announceURL (BDict b) =
    case b ! "announce" of BString str -> str
                           _ -> "error"
announceURL _ = "Error: Not properly Bencoded file."

initiateTrackerRequest :: String -> IO String
initiateTrackerRequest url =
    simpleHTTP (getRequest url) >>= getResponseBody

createTrackerRequest :: Bencode -> String
createTrackerRequest b =
    addKeyValuePairToRequest "info_hash" (infoHash b)
    (addKeyValuePairToRequest "peer_id" peerId
    (addKeyValuePairToRequest "port" listeningPort
    (addKeyValuePairToRequest "uploaded" initiallyUploaded
    (addKeyValuePairToRequest "downloaded" initiallyDownloaded
    (addKeyValuePairToRequest "left" (leftToDownload b)
    (addKeyValuePairToRequest "compact" compact
    (addKeyValuePairToRequest "no_peer_id" noPeerId
    (addKeyValuePairToRequest "event" initialEvent
    (setupGetRequest (announceURL b))))))))))

setupGetRequest :: String -> String
setupGetRequest url = url ++ "?"

addKeyValuePairToRequest :: String -> String -> String -> String
addKeyValuePairToRequest key value url = url ++ if endswith "?" url
                                                then key ++ "=" ++ value
                                                else "&" ++ key ++ "=" ++ value

-- helper function for infoHash: inserts y every n elements of xs.
insertEvery :: Eq a => Int -> a -> [a] -> [a]
insertEvery n y xs = countdown n xs where
    countdown _ [] = [] -- base case
    countdown m (x:xs) | m == n = y:countdown (n-1) (x:xs)
                       | m == 0 = x:countdown n xs
                       | otherwise = x:countdown (m-1) xs

infoHash :: Bencode -> String
infoHash (BDict b) = insertEvery 2 '%' (read (show (HexEncode.encode (SHA1.hash (B.pack (bUnparse (b ! "info")))))))
peerId :: String
peerId = "aaaaabbbbbcccccddddd"
listeningPort :: String
listeningPort = "6881"
initiallyUploaded :: String
initiallyUploaded = "0"
initiallyDownloaded :: String
initiallyDownloaded = "0"
leftToDownload :: Bencode -> String
leftToDownload _ = "10000"
compact :: String
compact = "0"
noPeerId :: String
noPeerId = "0"
initialEvent :: String
initialEvent = "started"

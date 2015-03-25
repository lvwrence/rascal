import System.Environment (getArgs)
import Network.HTTP
import Data.Maybe
import Data.List.Utils
import Data.Map
import Bencode

main :: IO ()
main =
    getArgs >>= \args ->
    readFile (head args) >>= \contents ->
    requestTracker (fromJust (bParse contents)) >>= \resp ->
    putStrLn resp


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

infoHash :: Bencode -> String
infoHash _ = "test"
peerId :: String
peerId = "aaaaabbbbbcccccdddddd"
listeningPort :: String
listeningPort = "6881"
initiallyUploaded :: String
initiallyUploaded = "0"
initiallyDownloaded :: String
initiallyDownloaded = "0"
leftToDownload :: Bencode -> String
leftToDownload _ = "9999999999"
compact :: String
compact = "0"
noPeerId :: String
noPeerId = "0"
initialEvent :: String
initialEvent = "started"

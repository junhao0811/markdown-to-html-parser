{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

-- import Assignment (markdownParser)

import           Assignment              (convertADTHTML, markdownParser, getTime)
import           Data.Aeson              (object, (.=))
import           Data.Aeson.Key          (fromString)
import           Data.Text.Lazy          (Text, pack, unpack)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Instances               (ParseResult (Result), parse)
import           Web.Scotty              (ActionM, body, json, post, scotty)

getResult :: ParseResult a -> (a -> String) -> String
getResult (Result _ a) f = f a
getResult _ _            = ""

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: Show a => [(String, a)] -> ActionM ()
jsonResponse pairs =
  json $ object [fromString key .= ((pack $ show value) :: Text) | (key, value) <- pairs]

-- Function to save the HTML to a file with the current timestamp
saveHTMLToFile :: String -> IO ()
saveHTMLToFile html = do
  time <- getTime
  -- Construct the file path using the current timestamp
  let filePath = "html/" ++ time ++ ".html"
  -- Write the HTML content to the file
  writeFile filePath html

main :: IO ()
main = scotty 3000 $ do
  post "/api/convertMD" $ do
    requestBody <- body
    -- Convert the raw request body from ByteString to Text
    let requestBodyText = decodeUtf8 requestBody
        -- Convert the Text to String
        str = unpack requestBodyText
        -- Parse the Markdown string using 'markdownParser' and apply 'convertAllHTML'
        converted_html = getResult (parse markdownParser str) convertADTHTML

    -- Respond with the converted HTML as JSON
    jsonResponse [("html", converted_html)]

-- Handling a POST request to save HTML
handleSaveHTMLRequest :: String -> IO ()
handleSaveHTMLRequest = saveHTMLToFile

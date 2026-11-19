{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module MarkdownParser (markdownParser, convertADTHTML, getTime) where

import Control.Applicative (Alternative (..), optional)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Instances (Parser (..))
import Parser
  ( satisfy, isNot, noneof, spaces, spaces1, inlineSpace, underscoreTok,
    doubleAsteriskTok, doubleTildeTok, openBracketTok, closeBracketTok,
    openParenthesisTok, closeParenthesisTok, backtickTok, openBracket,
    closeBracket, caret, positiveInt, exclamationMark, doubleQuoteTok,
    hash, twoHashes, threeHashes, fourHashes, fiveHashes, sixHashes,
    newline, equals2, dashes2, dashes3, greaterThan, threeBackticks,
    one, colonTok, fullStop, fourSpaces, pipe, sepBy1 )

-- Data type representing various Markdown elements
data ADT
  = Italic String
  | Bold String
  | Strikethrough String
  | Link String String
  | InlineCode String
  | Footnote Int
  | Image String String String
  | FootnoteReference Int String
  | RegularText String
  | FreeText [ADT]
  | Heading Int String
  | Blockquote [ADT]
  | Code (Maybe String) String
  | OrderedList [ADT]
  | Table ADT [ADT]
  | Markdown [ADT]
  deriving (Show, Eq)

-- Parser for italic text
parseItalic :: Parser ADT
parseItalic = Italic <$> (underscoreTok *> some (isNot '_') <* underscoreTok)

-- Parser for bold text
parseBold :: Parser ADT
parseBold = Bold <$> (doubleAsteriskTok *> some (noneof "**")
            <* doubleAsteriskTok)

-- Parser for strikethrough text
parseStrikethrough :: Parser ADT
parseStrikethrough = Strikethrough <$> (doubleTildeTok *> some (noneof "~~")
                    <* doubleTildeTok)

-- Parser for URLs
parseURL :: Parser String
parseURL = many (noneof "\t\r\f\v ")

-- Parser for links
parseLink :: Parser ADT
parseLink = Link <$> (openBracketTok *> some (isNot ']') <* closeBracketTok)
         <*> (openParenthesisTok *> parseURL <* closeParenthesisTok)

-- Parser for inline code
parseInlineCode :: Parser ADT
parseInlineCode = InlineCode <$>
                  (backtickTok *> some (isNot '`') <* backtickTok)

-- Parser for footnotes
parseFootnote :: Parser ADT
parseFootnote = Footnote <$>
                (openBracket *> caret *> positiveInt <* closeBracket)

-- Parser for text modifiers (italic, bold, strikethrough, link, inline code, footnote)
parseTextModifiers :: Parser ADT
parseTextModifiers = parseItalic <|> parseBold <|> parseStrikethrough
           <|> parseLink <|> parseInlineCode <|> parseFootnote

-- Parser for alternative text (used in images)
parseAltText :: Parser String
parseAltText = some (isNot ']')

-- Parser for caption text (used in images)
parseCaptionText :: Parser String
parseCaptionText = some (isNot '"')

-- Parser for images
parseImages :: Parser ADT
parseImages = Image <$> (inlineSpace *> exclamationMark *> openBracketTok
             *> parseAltText <* closeBracketTok)
          <*> (openParenthesisTok *> parseURL <* spaces1)
          <*> (doubleQuoteTok *> parseCaptionText <* doubleQuoteTok
             <* closeParenthesisTok)

-- Parser for references (used in footnotes)
parseReferences :: Parser String
parseReferences = some (isNot '\n')

-- Parser for footnote references
parseFootnotesReferences :: Parser ADT
parseFootnotesReferences = FootnoteReference <$> (inlineSpace *> openBracket
                          *> caret *> positiveInt
                          <* closeBracket <* colonTok
                          <* inlineSpace)
                       <*> parseReferences

-- Parser for regular text
-- parseText :: Parser ADT
-- parseText = do
--   text <- some (noneof "_**~~[]()^`!")
--   return $ RegularText text
parseRegularText :: Parser String
parseRegularText = some (satisfy (`notElem` "\n*~_[]^`!"))

parseText :: Parser ADT
parseText = do
  RegularText <$> parseRegularText

-- Parser for free text (combination of text and text modifiers)
parseFreeText :: Parser ADT
parseFreeText = do
  content <- some (parseTextModifiers <|> parseText)
  return $ FreeText content

-- Parser for free text with surrounding spaces
parseFreeTextFinal :: Parser ADT
parseFreeTextFinal = spaces *> parseFreeText <* spaces

-- Convert ADT to plain text
convertToPlainText :: ADT -> String
convertToPlainText (Italic value) = value
convertToPlainText (Bold value) = value
convertToPlainText (Strikethrough value) = value
convertToPlainText (Link text _) = text
convertToPlainText (InlineCode value) = value
convertToPlainText (Footnote number) = "[^" ++ show number ++ "]"
convertToPlainText (RegularText value) = value
convertToPlainText (FreeText content) = concatMap convertToPlainText content -- Using concatMap to convert each element to plain text and concatenate
convertToPlainText _ = ""

-- Parser for heading level 1
parseHeading1 :: Parser ADT
parseHeading1 = do
  _ <- inlineSpace
  _ <- hash
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 1 content
  <|> do
  contentADT <- parseFreeText
  _ <- newline
  _ <- inlineSpace
  _ <- equals2
  let content = convertToPlainText contentADT
  return $ Heading 1 content

-- Parser for heading level 2
parseHeading2 :: Parser ADT
parseHeading2 = do
  _ <- inlineSpace
  _ <- twoHashes
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 2 content
  <|> do
  contentADT <- parseFreeText
  _ <- newline
  _ <- inlineSpace
  _ <- dashes2
  let content = convertToPlainText contentADT
  return $ Heading 2 content

-- Parser for heading level 3
parseHeading3 :: Parser ADT
parseHeading3 = do
  _ <- inlineSpace
  _ <- threeHashes
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 3 content

-- Parser for heading level 4
parseHeading4 :: Parser ADT
parseHeading4 = do
  _ <- inlineSpace
  _ <- fourHashes
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 4 content

-- Parser for heading level 5
parseHeading5 :: Parser ADT
parseHeading5 = do
  _ <- inlineSpace
  _ <- fiveHashes
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 5 content

-- Parser for heading level 6
parseHeading6 :: Parser ADT
parseHeading6 = do
  _ <- inlineSpace
  _ <- sixHashes
  _ <- spaces1
  contentADT <- parseFreeText
  let content = convertToPlainText contentADT
  return $ Heading 6 content

-- Parser for any heading level
parseHeading :: Parser ADT
parseHeading = parseHeading1 <|> parseHeading2 <|> parseHeading3
         <|> parseHeading4 <|> parseHeading5 <|> parseHeading6

-- Parser for blockquotes
parseBlockquote :: Parser ADT
parseBlockquote = do
  quotes <- some (inlineSpace *> greaterThan *> inlineSpace *> parseFreeText)
  return $ Blockquote quotes

-- Parser for optional language identifier in code blocks
parseOptionalLanguageIdentifier :: Parser String
parseOptionalLanguageIdentifier = many (isNot '\n')

-- Parser for remaining code in code blocks
parseRemainingCode :: Parser String
parseRemainingCode = some (noneof "```")

-- Parser for code blocks
parseCode :: Parser ADT
parseCode = do
  lang <- optional (inlineSpace *> threeBackticks *> inlineSpace
          *> parseOptionalLanguageIdentifier <* newline)
  code <- parseRemainingCode
  _ <- threeBackticks
  return $ Code lang code

-- Parser for sublists in ordered lists
parseSublist :: Parser ADT
parseSublist = fourSpaces *> parseOrderedList 1

-- Parser for ordered list items
parseOrderedListItem :: Int -> Parser ADT
parseOrderedListItem index = do
  _ <- one
  _ <- positiveInt
  _ <- fullStop
  _ <- spaces1
  content <- parseFreeText
  return $ OrderedList [RegularText (show index ++ ". "
                   ++ convertToPlainText content)]

-- Parser for ordered lists
parseOrderedList :: Int -> Parser ADT
parseOrderedList index = do
  items <- some (parseOrderedListItem index <|> parseSublist)
  return $ OrderedList items

-- Parser for table cells
parseCell :: Parser ADT
parseCell = do
  _ <- inlineSpace
  content <- parseFreeText
  _ <- inlineSpace
  return content

-- Parser for table columns
parseColumn :: Parser [ADT]
parseColumn = sepBy1 parseCell pipe

-- Parser for table rows
parseRow :: Parser ADT
parseRow = do
  _ <- inlineSpace
  _ <- pipe
  columns <- parseColumn
  _ <- pipe <* newline
  return $ FreeText columns

-- Parser for table content
parseContent :: Parser [ADT]
parseContent = many parseRow

-- Parser for tables
parseTable :: Parser ADT
parseTable = do
  _ <- inlineSpace
  _ <- pipe
  header <- parseColumn
  _ <- pipe <* newline
  _ <- inlineSpace
  _ <- pipe
  _ <- sepBy1 dashes3 pipe <* newline
  rows <- parseContent
  return $ Table (FreeText header) rows

-- Main parser for Markdown documents
markdownParser :: Parser ADT
markdownParser = do
  Markdown <$> parseElements

-- Parser for various Markdown elements
parseElements :: Parser [ADT]
parseElements = do
  many (parseHeading <|> parseTextModifiers <|> parseImages
    <|> parseFootnotesReferences <|> parseFreeTextFinal
    <|> parseBlockquote <|> parseCode <|> parseOrderedList 1
    <|> parseTable)

-- Function to get the current time in a specific format
getTime :: IO String
getTime = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" <$> getCurrentTime

-- Convert ADT to HTML
convertADTHTML :: ADT -> String
convertADTHTML a = frontOfHTML ++ prettyPrintHTML 1 a ++ backOfHTML

-- HTML header
frontOfHTML :: String
frontOfHTML = "<!DOCTYPE html>\n<html lang=\"en\">\n\n<head>\n    "
              ++ "<meta charset=\"UTF-8\">\n    "
              ++ "<title>Test</title>\n</head>\n\n<body>\n"

-- HTML footer
backOfHTML :: String
backOfHTML = "\n</body>\n\n</html>"

-- Indentation helper function
indent :: Int -> String
indent indentLevel = replicate (4 * indentLevel) ' '

-- Pretty print ADT to HTML
prettyPrintHTML :: Int -> ADT -> String
prettyPrintHTML indentLevel (Markdown elements) =
  -- Recursively pretty print each element in the Markdown
  concatMap (prettyPrintHTML indentLevel) elements

prettyPrintHTML indentLevel (Italic value) =
  -- Convert Italic text to HTML <em> tag
  indent indentLevel ++ "<em>" ++ value ++ "</em>"

prettyPrintHTML indentLevel (Bold value) =
  -- Convert Bold text to HTML <strong> tag
  indent indentLevel ++ "<strong>" ++ value ++ "</strong>"

prettyPrintHTML indentLevel (Strikethrough value) =
  -- Convert Strikethrough text to HTML <del> tag
  indent indentLevel ++ "<del>" ++ value ++ "</del>"

prettyPrintHTML indentLevel (Link text url) =
  -- Convert Link to HTML <a> tag
  indent indentLevel ++ "<a href=\"" ++ url ++ "\">" ++ text ++ "</a>"

prettyPrintHTML indentLevel (InlineCode value) =
  -- Convert InlineCode to HTML <code> tag
  indent indentLevel ++ "<code>" ++ value ++ "</code>"

prettyPrintHTML indentLevel (Footnote number) =
  -- Convert Footnote to HTML <sup> tag with a link
  indent indentLevel ++ "<sup><a id=\"fn" ++ show number ++ "ref\" href=\"#fn"
  ++ show number ++ "\">" ++ show number ++ "</a></sup>"

prettyPrintHTML indentLevel (Image alt url caption) =
  -- Convert Image to HTML <img> tag
  indent indentLevel ++ "<p><img src=\"" ++ url ++ "\" alt=\"" ++ alt
  ++ "\" title=\"" ++ caption ++ "\"></p>"

prettyPrintHTML indentLevel (FootnoteReference number reference) =
  -- Convert FootnoteReference to HTML <p> tag with an id
  indent indentLevel ++ "<p id=\"fn" ++ show number ++ "\">" ++ reference
  ++ "</p>"

prettyPrintHTML indentLevel (FreeText value) =
  -- Convert FreeText to HTML <p> tag
  indent indentLevel ++ "<p>" ++ concatMap convertToPlainText value ++ "</p>"

prettyPrintHTML indentLevel (Heading level content) =
  -- Convert Heading to HTML <h1> to <h6> tag based on level
  indent indentLevel ++ "<h" ++ show level ++ ">" ++ content ++ "</h"
  ++ show level ++ ">"

-- Pretty print Blockquote to HTML <blockquote> tag
prettyPrintHTML indentLevel (Blockquote value) =
  -- Open blockquote tag with indentation
  indent indentLevel ++ "<blockquote>\n" ++
  -- Recursively pretty print each element in the Blockquote with increased indentation
  concatMap (prettyPrintHTML (indentLevel + 1)) value ++
  -- Close blockquote tag with indentation
  indent indentLevel ++ "</blockquote>\n"

prettyPrintHTML indentLevel (Code (Just lang) code) =
  -- Convert Code with language to HTML <pre><code> tag with class
  indent indentLevel ++ "<pre><code class=\"language-" ++ lang ++ "\">"
  ++ trimNewine code ++ "</code></pre>"

prettyPrintHTML indentLevel (Code Nothing code) =
  -- Convert Code without language to HTML <pre><code> tag
  indent indentLevel ++ "<pre><code>" ++ code ++ "</code></pre>"

prettyPrintHTML indentLevel (OrderedList items) =
  -- Convert OrderedList to HTML <ol> tag
  indent indentLevel ++ "<ol>" ++
  -- Recursively pretty print each item in the OrderedList and concatenate the results
  concatMap (prettyPrintHTML (indentLevel + 1)) items ++
  indent indentLevel ++ "</ol>"

prettyPrintHTML indentLevel (Table header rows) =
  -- Convert Table to HTML <table> tag with <thead> and <tbody>
  indent indentLevel ++ "<table>" ++
  indent (indentLevel + 1) ++ "<thead>" ++
  prettyPrintHeader (indentLevel + 2) header ++
  indent (indentLevel + 1) ++ "</thead>" ++
  indent (indentLevel + 1) ++ "<tbody>" ++
  prettyPrintRows (indentLevel + 2) rows ++
  indent (indentLevel + 1) ++ "</tbody>" ++
  indent indentLevel ++ "</table>"

-- Pretty print table header
prettyPrintHeader :: Int -> ADT -> String
prettyPrintHeader indentLevel (FreeText cells) =
  -- Convert table header cells to HTML <th> tags
  indent indentLevel ++ "<tr>" ++
  concatMap (\cell -> indent (indentLevel + 1) ++ "<th>"
            ++ convertToPlainText cell ++ "</th>") cells ++
  indent indentLevel ++ "</tr>"

-- Pretty print table rows
prettyPrintRows :: Int -> [ADT] -> String
prettyPrintRows indentLevel = concatMap (prettyPrintRow indentLevel)

-- Pretty print a single table row
prettyPrintRow :: Int -> ADT -> String
prettyPrintRow indentLevel (FreeText cells) =
  -- Convert table row cells to HTML <td> tags
  indent indentLevel ++ "<tr>" ++
  concatMap (\cell -> indent (indentLevel + 1) ++ "<td>"
            ++ convertToPlainText cell ++ "</td>") cells ++
  indent indentLevel ++ "</tr>"

-- Helper function to trim trailing newline characters
-- use reverse because dropWhile is from the beginning
trimNewine :: String -> String
trimNewine = reverse . dropWhile (== '\n') . reverse
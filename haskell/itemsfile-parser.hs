import Control.Applicative hiding (many, (<|>))
import Text.ParserCombinators.Parsec

import CountItems

p_file :: CharParser () [String]
p_file = concat <$> endBy p_line eol
    where eol = try (string "\n\r") <|> string "\n"

p_line :: CharParser () [String]
p_line = between (string "\"[") (string "]\"") p_lineItems
        <|> between (char '[') (char ']') p_lineItems

p_lineItems :: CharParser () [String]
p_lineItems = sepBy1 item (char ',')
    where item = many1 (noneOf ",]")

main =
    do c <- getContents
       case parse p_file "(stdin)" c of
            Left e -> do putStrLn "Error parsing input:"
                         print e
            Right items -> (printSorted . countItems) items
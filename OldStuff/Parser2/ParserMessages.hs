module OldStuff.Parser2.ParserMessages where

import Text.ParserCombinators.Parsec.Pos(SourcePos, sourceName)

data ParserError
  = ParserUnexpectedEOF
  | ParserUnexpectedChar SourcePos Char
  | ParserMismatchingBrackets SourcePos Char SourcePos Char

getParserMessage :: ParserError -> String
getParserMessage ParserUnexpectedEOF = "Unexpected end of file."
getParserMessage (ParserUnexpectedChar pos c) = "unexpected char " ++ [c] ++ " add position " ++ sourceName pos
getParserMessage (ParserMismatchingBrackets s1 c1 s2 c2) = "mismatching brackets at pos " ++ sourceName s1 ++ " char " ++ [c1] ++ " and pos " ++ sourceName s2 ++ " char " ++ [c2]

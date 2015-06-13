{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Radiation.Parsers.Languages.CPP(parser) where

import Control.Applicative ((<$>), (<|>), many, (<*), pure)
import Control.Monad
import Data.Attoparsec.ByteString.Char8 as BP
import Data.Char (isAlphaNum, isDigit, isAlpha)
import Debug.Trace
import My.Utils
import Prelude hiding (log)
import Radiation.Parsers.Internal.CStyle
import Radiation.Parsers.Internal.CppStyle
import Radiation.Parsers.Internal.CommandParser
import Radiation.Parsers.Internal.InternalIO
import Radiation.Parsers.Internal.WithAttoparsec
import Vim

import Data.ByteString (ByteString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Radiation.Parsers as R

blacklist :: Set.Set BS.ByteString
blacklist = Set.fromList [
            "auto"    ,"else"  ,"long"        ,"switch",
            "break"   ,"enum"  ,"register"    ,"typedef",
            "case"    ,"extern","return"      ,"union",
            "char"    ,"float" ,"short"       ,"unsigned",
            "const"   ,"for"   ,"signed"      ,"void",
            "continue","goto"  ,"sizeof"      ,"volatile",
            "default" ,"if"    ,"static"      ,"while",
            "do"      ,"int"   ,"struct"      ,"_Packed",
            "double"  ,"asm"   ,"dynamic_cast","namespace","reinterpret_cast",  "try",
            "bool",        "explicit",      "new",        "static_cast",       "typeid",
            "catch",       "false",         "operator",   "template",          "typename",
            "class",       "friend",        "private",    "this",              "using",
            "const_cast",  "inline",        "public",     "throw",             "virtual",
            "delete",      "mutable",       "protected",  "true",              "wchar_t" ]

typMap :: Map.Map BS.ByteString BS.ByteString
typMap = Map.fromList [
        ("struct","RadiationCppStruct"),
        ("union","RadiationCppUnion"),
        ("class","RadiationCppClass"),
        ("enum","RadiationCppEnum")]

data CPPConfig = CPPConfig {
    configIncludeMemberFunctions :: Bool
}

{- Parser the C++ file. Look for classes, typedefs and namespaces -}
parseCPP :: CPPConfig -> Parser (Map.Map String (Set.Set BS.ByteString))
parseCPP (CPPConfig parseMembers) = 
    let
        maybeP parser = (Just <$> parser) <|> return Nothing
        word = skipSpace >> BP.takeWhile sat <* skipSpace
            where sat ch = isAlphaNum ch || ch == '_'


        parseClassBody :: BS.ByteString -> Parser [(String, ByteString)]
        parseClassBody = subparse $ do
                let memberFn = do
                        _type <- spaced cppType
                        name <- identifier
                        _parens <- spaced balancedParens
                        return [("RadiationCppMemberFunction", name)]
                concat <$> many (memberFn <|> one)


        parseClass :: Parser [(String, ByteString)]
        parseClass = do
            {- Try to just parse a class. get the name and soon we will get into the internals of the class  -}
            clazz <- string "class" >> maybeP (spaced attribute) >> ("RadiationCppClass",) <$> word
            skipSpace
            {- Either this is a forward declaration, or we have a body  -}
            (<|>) (char ';' $> [clazz]) $ do
                  bp <- BP.takeWhile (\c -> c /= '{' && c /= ';')
                  bod <- body
                  (clazz:) <$> parseClassBody bod

        parseTemplate :: Parser [(String, ByteString)]
        parseTemplate =
            let parseInside _ = [] in
            parseInside <$> (string "template" >> skipSpace >> balanced '<' '>')

        {- Parse a single element. Either a class, sturct or union. -}
        parseElement :: Parser [(String, ByteString)]
        parseElement =
            parseTemplate <|> parseClass <|>
            (fmap return $ string "struct"      >> maybeP (spaced attribute) >> ("RadiationCppStruct",)    <$> word) <|>
            (fmap return $ string "union"       >> maybeP (spaced attribute) >> ("RadiationCppUnion",)     <$> word) <|>
            (fmap return $ string "enum"        >> maybeP (spaced attribute) >> ("RadiationCppEnum",)      <$> word) <|>
            (fmap return $ string "namespace"   >> maybeP (spaced attribute) >> ("RadiationCppNamespace",) <$> word) <|>
            (fmap return $ string "typedef"     >> maybeP (spaced attribute) >> ("RadiationCppTypedef",)   <$> typedef)

        typedef = do
            void (skipSpace >> string "struct" >> skipSpace >> (body <|> (identifier >> body) <|> identifier)) <|> return ()
            bs <- BP.takeWhile (/=';')
            return $ last $ BSC.words bs

        one :: Parser [(String, ByteString)]
        one =
              {- Try to parse a class -}
              parseElement <|> 
              {- take until the next space -}
              (BP.takeWhile1 (not . isSpace) >> BP.takeWhile1 isSpace $> []) <|>
              {- take any char otherwise -}
              (anyChar $> [])
        in

        (map_fromList2 . concat) <$> many one

parser :: R.Parser
parser = R.Parser "cpp" (const ["g:radiation_parse_members",    
                                "g:radiation_cpp_cc",
                                "g:radiation_cpp_flags"])
    $ \filename -> do
        log Info "Start cpp parser"
    
        forM_ (map snd $ Map.toList typMap) $ flip R.hiLink "Type"
        R.hiLink "RadiationCppMemberFunction" "Function"
    
        {- Get the utilities to parse the output -}
        pipes <- runCommand =<< sequence
            [queryDefault "g:radiation_cpp_cc" "g++",
             queryDefault "g:radiation_cpp_flags" "",
             pure "-E", pure filename]
        
        config <- CPPConfig <$> ((=="1") <$> queryDefault "g:radiation_parse_members" "1")
        reportErrors pipes $
            withParsingMap (Map.map (Set.\\blacklist) <$> parseCPP config)
                <=< vGetHandleContents

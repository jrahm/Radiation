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


        parseClassBody :: BS.ByteString -> BS.ByteString -> Parser [(String, ByteString)]
        parseClassBody classname = subparse $ do
                let memberFn = do
                        _type <- spaced cppType
                        name <- identifier
                        if name == classname then
                            {- This means that this is a constructor. Don't parse it -}
                            fail ""
                            else do
                                _parens <- spaced balancedParens
                                return [("RadiationCppMemberFunction", name)]
                concat <$> many (memberFn <|> one)


        parseClass :: Parser [(String, ByteString)]
        parseClass = do
            {- Try to just parse a class. get the name and soon we will get into the internals of the class  -}
            clazz <- string "class" >> maybeP (spaced attribute) >> ("RadiationCppClass",) <$> word
            if not parseMembers then
                {- If we are not supposed to parse members, then
                 - just continue without consuming the body -}
                    return [clazz]
                else do
                    {- If we are supposed to parse the body, then continue
                     - consuming the body and parsing it. -}
                    skipSpace
                    {- Either this is a forward declaration, or we have a body  -}
                    (<|>) (char ';' $> [clazz]) $ do
                          bp <- BP.takeWhile (\c -> c /= '{' && c /= ';')
                          bod <- body
                          (clazz:) <$> parseClassBody (snd clazz) bod

        parseTemplate :: Parser [(String, ByteString)]
        parseTemplate =
            let parseInside :: Parser [(String, ByteString)]
                parseInside = 
                 (concat<$>) . many $ (fmap return $ spaced (string "typename") >> ("RadiationCppTemplateTypename",) <$> identifier) <|>
                                      (fmap return $ spaced (string "class")    >> ("RadiationCppTemplateClass",)    <$> identifier) <|>
                                      (nextToken $> [])

            in
            subparse parseInside =<< (string "template" >> skipSpace >> balanced '<' '>')

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
        R.hiLink "RadiationCppMemberFunction"    "Function"
        R.hiLink "RadiationCppTemplateClass"     "Type"
        R.hiLink "RadiationCppTemplateTypename"  "Type"
    
        {- Get the utilities to parse the output -}
        pipes <- runCommand =<< sequence
            [queryDefault "g:radiation_cpp_cc" "g++",
             queryDefault "g:radiation_cpp_flags" "",
             pure "-E", pure filename]
        
        config <- CPPConfig <$> ((=="1") <$> queryDefault "g:radiation_parse_members" "1")
        reportErrors pipes $
            withParsingMap (Map.map (Set.\\reservedWords) <$> parseCPP config)
                <=< vGetHandleContents

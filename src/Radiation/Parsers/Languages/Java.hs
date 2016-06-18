{-# LANGUAGE OverloadedStrings #-}
module Radiation.Parsers.Languages.Java (Radiation.Parsers.Languages.Java.parser) where

import Control.Applicative
import Control.Monad hiding (forM_)
import Control.Monad.IO.Class

import Data.Char
import Data.Foldable (forM_)
import Data.List
import Data.List.Split
import Data.Maybe

import Language.Java.Lexer
import Language.Java.Parser as J
import Language.Java.Pretty
import Language.Java.Syntax

import Prelude hiding (log)

import Radiation.Parsers.Internal.CommandParser
import Radiation.Parsers.Internal.InternalIO
import Radiation.Parsers.Languages.JavaLang

import System.Directory (findFile, getModificationTime)
import System.Exit
import System.Process hiding (runCommand)

import Vim

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Set as Set
import qualified Radiation.Parsers as R

import Data.Time
import Data.Time.Format

import System.Locale


parseFromSource :: [String] -> [String] -> VimM Bool
parseFromSource classname classpaths =
  let sourcefile = (intercalate "/" classname) ++ ".java"
  in do
	    -- source <- liftIO (findFile classpaths sourcefile)
      source <- liftIO (findFile classpaths sourcefile)
      cacheKey <-
        case source of 
          Nothing -> return ""
          Just s -> do
                modTime <- liftIO $ getModificationTime s
                return (formatTime defaultTimeLocale "%F%T" modTime)

      log Info (BSC.pack $ "Found file: " ++ show source)
      runTryCache cacheKey $ do
        forM_ source $ parse False
        return (isJust source)

highlightLang :: VimM ()
highlightLang = do
  void $ runTryCache "java.lang.*" $ do
    parVim $
      flip map javaLang $ \clazz -> do
        highlightFromImport (ImportDecl False (Name $ map Ident ["java", "lang", clazz]) False)
    return True

highlightFromImport :: ImportDecl -> VimM ()
highlightFromImport (ImportDecl _ (Name name) _) =
  let classname = intercalate "." (map toStr name) in do
  classpath <- query "g:radiation_java_classpath"

  {- Try to find the class somewhere in a source file
   - on the classpath somewhere. -}
  srcInFile <- parseFromSource (map toStr name)
                ("." : (fromMaybe [] $ splitOn ":" <$> classpath))

  {- The class was not found in a file, so we must
   - continue onto using javap to find the source. -}
  when (not srcInFile) $
    void $ runTryCache classname $ do
             pipes@(_, _, ph) <- runCommand =<<
                     sequence [
                             queryDefault "g:radiation_java_javap" "javap"
                             , queryDefault "g:radiation_java_javap_flags" ""
                             , pure "-classpath"
                             , pure (fromMaybe "''" classpath)
                             , pure "-constants"
                             , pure classname
                     ]
             
             reportErrors pipes $ \stdout -> do
               contents' <- vGetHandleContents stdout

               let contents = lines contents'

               case contents of
                 (_:rest) ->
                   let everything = unlines (map sanitize rest)
                       result = J.parser compilationUnit everything
                   in do
                     log Info (BSC.pack everything)
                     case result of
                       Left e -> log Error (BSC.pack $ show e)
                       Right ast -> walkCompilation False ast
                     return True
               
                 _ -> do
                   log Info "javap returned bad exit code. Try to find source"
                   return False

  where
    toStr (Ident x) = x

    delete x (a:as) | x == a = delete x as
    delete x (a:as) = a:delete x as
    delete x [] = []

    deleteParens ('(':xs) = "(){}"
    deleteParens (x:xs) = x : deleteParens xs
    deleteParens [] = []

    replaceIntf ("interface":xs) = "class":replaceIntf xs
    replaceIntf (a:as) = a:replaceIntf as
    replaceIntf [] = []

    issep c = isAlphaNum c  || c == '.' || c == '$' || c == '_' || c == '/' || c == '-'

    mywords [] = []
    mywords xs =
        let (a,r) = break (not . issep) xs in
          a:mywords' r

    mywords' [] = []
    mywords' xs =
        let (a, r) = break issep xs in
          a:mywords r

    fixQualified line =
      unwords $ map (last . splitOn "/") $
                map (last . splitOn ".")
                (replaceIntf $ mywords line)

    sanitize line = fixQualified (deleteParens line)

walkCompilation :: Bool -> CompilationUnit -> VimM ()
walkCompilation runImports (CompilationUnit _ imports typeDecl) = do
  when runImports $
    void $ do
      highlightLang
      parVim (map highlightFromImport imports)
  mapM_ walkTypeDecl typeDecl


walkClassDecl (ClassDecl _ (Ident name) _ _ _ body) = do
  R.synKeywordS "javaType" [name]
  walkClassBody body

walkInterfaceDecl (InterfaceDecl _ (Ident name) _ _ body) = do
  R.synKeywordS "javaType" [name]
  walkInterfaceBody body

walkInterfaceBody (InterfaceBody decls) =
  mapM_ walkMemberDecl decls

walkClassBody (ClassBody decls) =
  forM_ decls walkDecl

walkDecl (MemberDecl x) = walkMemberDecl x
walkDecl _ = return ()

walkMemberDecl (MethodDecl _ _ _ (Ident name) _ _ _) =
  R.synKeywordS "Function" [name]

walkMemberDecl (MemberClassDecl member) = walkClassDecl member
walkMemberDecl (MemberInterfaceDecl member) = walkInterfaceDecl member
walkMemberDecl (FieldDecl modifiers _ vars) = do
  let modifierSet = Set.fromList modifiers

  when (Set.member Public modifierSet &&
        Set.member Static modifierSet &&
        Set.member Final  modifierSet)
         (R.synKeywordS "Constant" (map toStr vars))


  where
    toStr (VarDecl v _) = toStr' v

    toStr' (VarId (Ident name)) = name
    toStr' (VarDeclArray v) = toStr' v

walkMemberDecl _ = return ()

walkTypeDecl (ClassTypeDecl classDecl) = walkClassDecl classDecl
walkTypeDecl (InterfaceTypeDecl interfaceDecl) = walkInterfaceDecl interfaceDecl

parser :: R.Parser
parser = parser' True

parser' :: Bool -> R.Parser
parser' useImports =
    R.Parser "java"
      (const ["g:radiation_java_classpath"])
      (parse useImports)

parse :: Bool -> FilePath -> VimM ()
parse useImports filename = do
      
        source <- liftIO $ readFile filename
        let result = J.parser compilationUnit source
        case result of
          Left e -> log Error (BSC.pack $ show e)
          Right ast -> walkCompilation useImports ast
  
                  

{-
    This empty file is provided for ease of patching in site specific checks.
    However, there are no guarantees regarding compatibility between versions.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module ShellCheck.Checks.Custom (checker, ShellCheck.Checks.Custom.runTests) where

import ShellCheck.AnalyzerLib
import ShellCheck.ASTLib
import ShellCheck.AST
import ShellCheck.Interface
import ShellCheck.Data

import qualified Data.Map.Strict as Map
import Test.QuickCheck

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Data.Char



checker :: Parameters -> Checker
checker params = Checker {
    perScript = \(Root root) -> do
            tell $ concatMap (\f -> f params root) [checkNotCamelCaseVar],
    perToken = const $ return ()
  }

checkNotCamelCaseVar :: Parameters -> p -> [TokenComment]
checkNotCamelCaseVar params t = warning 
  where
    isLocal = any isLower
   
    containUnderscore = foldr ((||) . (== '_')) True

    toCamelCase :: String -> String
    toCamelCase ('_':xs) = toCamelCase' xs
    toCamelCase (x:xs) = x : toCamelCase xs
    toCamelCase _ = ""

    toCamelCase' (x:xs) =  toUpper x : toCamelCase xs
    toCamelCase' _ = ""

    warningForLocals var place = do
        return $ info (getId place) 5000 $
            "变量 " ++ var ++ " 建议使用驼峰命名, 考虑使用变量名 " ++ toCamelCase var ++ "."

    warningFor (var, place) = do
        guard $ isVariableName var
        guard $ containUnderscore var
        guard $ isLocal var
        warningForLocals var place

    varMap = execState (mapM tally $ variableFlow params) Map.empty
    defaultAssigned = Map.fromList $ map (, ()) $ filter (not . null) internalVariables

    tally (Reference (_, place, name)) =
        modify (Map.insertWith (const id) name place)
    tally _ = return ()

    allVars = Map.toList $ Map.difference varMap defaultAssigned

    
    warning = execWriter . sequence $ mapMaybe warningFor allVars

prop_CustomTestsWork = True

return []
runTests = $quickCheckAll

{-
    This empty file is provided for ease of patching in site specific checks.
    However, there are no guarantees regarding compatibility between versions.
-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module ShellCheck.Checks.Custom (checker, ShellCheck.Checks.Custom.runTests) where

import ShellCheck.AnalyzerLib
import ShellCheck.ASTLib
import ShellCheck.AST
import ShellCheck.Interface
import ShellCheck.Data
import ShellCheck.Regex

import qualified Data.Map.Strict as Map
import Test.QuickCheck

import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import Data.Maybe
import Data.Char
import qualified Data.Set as Set
import Data.Foldable
import Data.List
import qualified Data.List.NonEmpty as NE

checker :: Parameters -> Checker
checker params = Checker {
    perScript = \(Root root) -> do
            tell $ concatMap (\f -> f params root) treeChecks,
    perToken = const $ return ()
  }

treeChecks = [
    nodeChecksToTreeCheck nodeChecks
    ,checkNotCamelCaseVar
    ,checkCompareArgsNumber
    ]

nodeChecks :: [Parameters -> Token -> Writer [TokenComment] ()]
nodeChecks = [
    checkSetE
    ,checkDevRedirect
    -- ,checkCommandResult
    ]

runNodeAnalysis f p t = execWriter (doAnalysis (f p) t)
nodeChecksToTreeCheck checkList =
    runNodeAnalysis
        (\p t -> mapM_ ((\ f -> f t) . (\ f -> f p))
            checkList)

checkNotCamelCaseVar :: Parameters -> p -> [TokenComment]
checkNotCamelCaseVar params _ = 
    execWriter . sequence $ mapMaybe warningFor allVars 
  where
    isLocal = any isLower
   
    containUnderscore = foldr ((||) . (== '_')) False

    toCamelCase :: String -> String
    toCamelCase ('_':xs) = toCamelCase' xs
    toCamelCase (x:xs) = x : toCamelCase xs
    toCamelCase _ = ""

    toCamelCase' (x:xs) =  toUpper x : toCamelCase xs
    toCamelCase' _ = ""

    warningForLocals var place = do
        return $ style (getId place) 5001 $
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

checkSetE params (T_Annotation _ _ (T_Script id _ _)) = 
    unless (hasSetE params) $ warn id 5002 "建议设置 set -e 选项, 避免后续脚本在错误命令状态下继续执行"
checkSetE _ _ = return ()

checkCompareArgsNumber params root@(T_Annotation _ _ (T_Script rootScriptID _ _)) = do
    execWriter $ when (hasPositionalReferenceInRoot && not hasCompareArgsNumberInRoot) $ infoFor rootScriptID
  where
    infoFor id = warn id 5000 "检测到读取脚本参数, 建议通过比较 $# 的值检查传递给脚本的参数数量, 确保传入参数数量与预期一致"
    check' t = case t of
        T_DollarBraced id _ val -> getLiteralString val == Just "#"
        _ -> False
    
    check t = isDirectChildOfRoot t && case t of
        TC_Binary _ _ _ lhs _ -> case lhs of
            T_NormalWord _ [T_DoubleQuoted _ [word]] -> check' word
            T_NormalWord _ [word] -> check' word
            _ -> False
        _ -> False
    
    hasCompareArgsNumberInRoot = isNothing $ doAnalysis (guard . not . check) root

    isDirectChildOfRoot child = fromMaybe False $ do
        function <- find (\case
            T_Function {} -> True
            T_Script {} -> True
            _ -> False) $
                getPath (parentMap params) child
        return $ rootScriptID == getId function

    isPositional str = str == "*" || str == "@" || str == "#"
        || (all isDigit str && str /= "0" && str /= "")

    isPositionalReferenceInRoot x =
        case x of
            Reference (_, t, str) -> isPositional str && isDirectChildOfRoot t
            _ -> False
    hasPositionalReferenceInRoot = any isPositionalReferenceInRoot $ variableFlow params
checkCompareArgsNumber _ _ = execWriter $ return ()

checkDevRedirect params redir@(T_Redirecting _ [
    T_FdRedirect _ _ (T_IoFile _ _ file)
    ] _) =  when (isDev file) $ error file
  where
    isDev t = case t of
        T_NormalWord _ [T_Literal _ str] -> str `isPrefixOf` "/dev/sd"
        _ -> False

    error t = warn (getId t) 6010 "高危命令检测: 重定向到 /dev/sd*"
checkDevRedirect _ _ = return ()

checkCommandResult params token =
    case token of
        TC_Binary id _ op lhs rhs -> check lhs rhs
        TA_Binary id op lhs rhs
            | op `elem` [">", "<", ">=", "<=", "==", "!="] -> check lhs rhs
        -- TA_Sequence _ [exp]
        --     | isExitCode exp -> message (getId exp)
        _ -> return ()
  where
    -- We don't want to warn about composite expressions like
    -- [[ $? -eq 0 || $? -eq 4 ]] since these can be annoying to rewrite.
    isOnlyTestInCommand t =
        case NE.tail $ getPath (parentMap params) t of
            T_Condition {}:_ -> True
            T_Arithmetic {}:_ -> True
            TA_Sequence _ [_]:T_Arithmetic {}:_ -> True

            -- Some negations and groupings are also fine
            next@(TC_Unary _ _ "!" _):_ -> isOnlyTestInCommand next
            next@(TA_Unary _ "!" _):_ -> isOnlyTestInCommand next
            next@TC_Group {}:_ -> isOnlyTestInCommand next
            next@(TA_Sequence _ [_]):_ -> isOnlyTestInCommand next
            next@(TA_Parenthesis _ _):_ -> isOnlyTestInCommand next
            _ -> False

    -- TODO: Do better $? tracking and filter on whether
    -- the target command is in the same function
    getFirstCommandInFunction = f
      where
        f t = case t of
            T_Function _ _ _ _ x -> f x
            T_BraceGroup _ (x:_) -> f x
            T_Subshell _ (x:_) -> f x
            T_Annotation _ _ x -> f x
            T_AndIf _ x _ -> f x
            T_OrIf _ x _ -> f x
            T_Pipeline _ _ (x:_) -> f x
            T_Redirecting _ _ (T_IfExpression _ ((x:_,_):_) _) -> f x
            x -> x

    isFirstCommandInFunction = fromMaybe False $ do
        let path = getPath (parentMap params) token
        func <- find isFunction path
        cmd <- getClosestCommand (parentMap params) token
        return $ getId cmd == getId (getFirstCommandInFunction func)

    check lhs rhs = case (isExitCode lhs, isExitCode rhs) of
        (True, False) -> case getLiteralString rhs of
            Just code    -> message code (getId rhs)           
            _ -> return ()
        (False, True) -> case getLiteralString lhs of
            Just code    -> message code (getId rhs)           
            _ -> return ()
        (_, _) -> return ()

    isNotFount t = getLiteralString t == Just "127"
    isZero t = getLiteralString t == Just "0"
    isExitCode t =
        case getWordParts t of
            [T_DollarBraced _ _ l] -> concat (oversimplify l) == "?"
            _ -> False

    message code id = when (isOnlyTestInCommand token && not isFirstCommandInFunction) $ info id 5004 $ 
        "退出码检查, " ++ code ++ "表示" ++ case code of
            "0" -> "命令执行成功"
            "1" -> "一般性未知错误"
            "2" -> "错误使用命令参数"
            "126" -> "命令无法执行"
            "127" -> "找不到命令"
            _ -> "未知错误"

prop_CustomTestsWork = True

return []
runTests = $quickCheckAll

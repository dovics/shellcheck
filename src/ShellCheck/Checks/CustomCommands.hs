{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module ShellCheck.Checks.CustomCommands (checker, ShellCheck.Checks.CustomCommands.runTests) where


import ShellCheck.Interface
import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib

import qualified Data.Map.Strict as M
import Data.List
import Control.Monad

import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (quickCheckWithResult, stdArgs, maxSuccess)

data CommandName = Exactly String | Basename String
    deriving (Eq, Ord)

data CommandCheck =
    CommandCheck CommandName (Token -> Analysis)

commandChecks :: [CommandCheck]
commandChecks = [
    checkForceRm,
    checkUserdel,
    checkGroupdel,
    checkKillall,
    checkKill,
    checkReboot,
    checkShutdown,
    checkInit,
    checkMvDevNull
    ]

buildCommandMap :: [CommandCheck] -> M.Map CommandName (Token -> Analysis)
buildCommandMap = foldl' addCheck M.empty
  where
    addCheck map (CommandCheck name function) =
        M.insertWith composeAnalyzers name function map


checkCommand :: M.Map CommandName (Token -> Analysis) -> Token -> Analysis
checkCommand map t@(T_SimpleCommand id cmdPrefix (cmd:rest)) = sequence_ $ do
    name <- getLiteralString cmd
    return $
        if | '/' `elem` name ->
               M.findWithDefault nullCheck (Basename $ basename name) map t
           | name == "builtin", (h:_) <- rest ->
               let t' = T_SimpleCommand id cmdPrefix rest
                   selectedBuiltin = onlyLiteralString h
               in M.findWithDefault nullCheck (Exactly selectedBuiltin) map t'
           | otherwise -> do
               M.findWithDefault nullCheck (Exactly name) map t
               M.findWithDefault nullCheck (Basename name) map t

  where
    basename = reverse . takeWhile (/= '/') . reverse
checkCommand _ _ = return ()

getChecker :: [CommandCheck] -> Checker
getChecker list = Checker {
    perScript = const $ return (),
    perToken = checkCommand map
    }
  where
    map = buildCommandMap list

checker :: AnalysisSpec -> Parameters -> Checker
checker spec params = getChecker commandChecks
  where
    keys = asOptionalChecks spec

verify :: CommandCheck -> String -> Bool
verify f s = producesComments (getChecker [f]) s == Just True
verifyNot f s = producesComments (getChecker [f]) s == Just False

prop_checkCatastrophicRm1 = verify checkForceRm "rm -f $1$2"
checkForceRm = CommandCheck (Basename "rm") $ \t ->
    when (isForce t) $
        warn (getId t) 6001 "高危命令检测: 文件或文件夹强制删除."
  where
    isForce = any ((`elem` ["f"]) . snd) . getAllFlags

checkUserdel = CommandCheck (Basename "userdel") $ \t ->
    warn (getId t) 6002 "高危命令检测: 检测到用户删除命令(userdel)."

checkGroupdel = CommandCheck (Basename "groupdel") $ \t ->
    warn (getId t) 6003 "高危命令检测: 检测到用户组删除命令(groupdel)."

checkKillall = CommandCheck (Basename "killall") $ \t ->
    warn (getId t) 6004 "高危命令检测: 检测到进程终止命令(killall)."

checkKill = CommandCheck (Basename "kill") $ \t ->
    warn (getId t) 6005 "高危命令检测: 检测到进程终止命令(kill)."

checkReboot = CommandCheck (Basename "reboot") $ \t ->
    warn (getId t) 6006 "高危命令检测: 检测到节点重启命令(reboot)."

checkShutdown = CommandCheck (Basename "shuthown") $ \t ->
    warn (getId t) 6007 "高危命令检测: 检测到节点关闭命令(shuthown)."

checkInit = CommandCheck (Basename "init") $ \t ->
    warn (getId t) 6008 "高危命令检测: 检测到节点初始化命令(init)."

checkMvDevNull = CommandCheck (Basename "mv") checkDestination
  where
    checkDestination token = do
        case params token of
            [single] -> do
                case find (\(t,x) -> x /= "" && x `isPrefixOf` "target-directory") args of
                    Just (t, _) -> err (getId t) 6009 "高危命令检测: 检测到删除命令(mv .. --target-directory=/dev/null)"
                    _ -> return ()
            [source, target] -> 
                when (isDevNull target) $ err (getId target) 6009 "高危命令检测: 检测到删除命令(mv .. /dev/null)"
            _ -> return ()
        where
            args = getAllFlags token
            params t = [x | (x,"") <- args]
            isDevNull t =
                case t of
                    T_NormalWord _ [T_Literal  _ s] -> s == "/dev/null"
                    _ -> False
            hasTarget =
                any (\(t,x) -> x /= "" && x `isPrefixOf` "target-directory" && isDevNull t) args

return []
runTests =  $( [| $(forAllProperties) (quickCheckWithResult (stdArgs { maxSuccess = 1 }) ) |])

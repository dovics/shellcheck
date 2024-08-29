{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}

module ShellCheck.Checks.CustomCommands (checker, ShellCheck.Checks.CustomCommands.runTests) where

import Control.Monad
import Data.Aeson.Encoding (value)
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import GHC.RTS.Flags (TraceFlags (user))
import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib
import ShellCheck.Interface
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (maxSuccess, quickCheckWithResult, stdArgs)
import Text.Parsec (parse, token)

data CommandName = Exactly String | Basename String
  deriving (Eq, Ord)

data CommandCheck
  = CommandCheck CommandName (Token -> Analysis)

commandChecks :: [CommandCheck]
commandChecks =
  [ checkForceRm,
    checkUserdel,
    checkGroupdel,
    checkKillall,
    checkKill,
    checkReboot,
    checkShutdown,
    checkInit,
    checkMvDevNull,
    checkHalt,
    checkPoweroff,
    checkSystemctlStop,
    checkServiceStop,
    checkDd,
    checkSSHHost,
    checkSu,
    checkSudo,
    checkChmod
    -- ,checkTimeout
    -- checkExit
  ]
    ++ map checkMkfs mkfsCommandList

buildCommandMap :: [CommandCheck] -> M.Map CommandName (Token -> Analysis)
buildCommandMap = foldl' addCheck M.empty
  where
    addCheck map (CommandCheck name function) =
      M.insertWith composeAnalyzers name function map

checkCommand :: M.Map CommandName (Token -> Analysis) -> Token -> Analysis
checkCommand map t@(T_SimpleCommand id cmdPrefix (cmd : rest)) = sequence_ $ do
  name <- getLiteralString cmd
  return $
    if
      | '/' `elem` name ->
          M.findWithDefault nullCheck (Basename $ basename name) map t
      | name == "builtin",
        (h : _) <- rest ->
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
getChecker list =
  Checker
    { perScript = const $ return (),
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
    warn (getId t) 5100 "高危命令检测: 检测到文件或文件夹强制删除, 该命令为高危命令请谨慎使用."
  where
    isForce = any ((`elem` ["f", "force"]) . snd) . getAllFlags

checkUserdel = CommandCheck (Basename "userdel") $ \t ->
  warn (getId t) 5101 "高危命令检测: 检测到用户删除命令(userdel), 该命令为高危命令请谨慎使用."

checkGroupdel = CommandCheck (Basename "groupdel") $ \t ->
  warn (getId t) 5102 "高危命令检测: 检测到用户组删除命令(groupdel), 该命令为高危命令请谨慎使用."

checkKillall = CommandCheck (Basename "killall") $ \t ->
  warn (getId t) 5103 "高危命令检测: 检测到进程终止命令(killall), 该命令为高危命令请谨慎使用."

checkKill = CommandCheck (Basename "kill") $ \t ->
  warn (getId t) 5104 "高危命令检测: 检测到进程终止命令(kill), 该命令为高危命令请谨慎使用."

checkReboot = CommandCheck (Basename "reboot") $ \t ->
  warn (getId t) 5105 "高危命令检测: 检测到节点重启命令(reboot), 该命令为高危命令请谨慎使用."

checkShutdown = CommandCheck (Basename "shuthown") $ \t ->
  warn (getId t) 5106 "高危命令检测: 检测到节点关闭命令(shuthown), 该命令为高危命令请谨慎使用."

checkInit = CommandCheck (Basename "init") $ \t ->
  warn (getId t) 5107 "高危命令检测: 检测到节点初始化命令(init), 该命令为高危命令请谨慎使用."

checkMvDevNull = CommandCheck (Basename "mv") checkDestination
  where
    checkDestination token = do
      case params token of
        [single] -> do
          case find (\(t, x) -> x /= "" && x `isPrefixOf` "target-directory") args of
            Just (t, _) -> err (getId t) 5108 "高危命令检测: 检测到删除命令(mv .. --target-directory=/dev/null)"
            _ -> return ()
        [source, target] ->
          when (isDevNull target) $ err (getId target) 5108 "高危命令检测: 检测到删除命令(mv .. /dev/null)"
        _ -> return ()
      where
        args = getAllFlags token
        params t = [x | (x, "") <- args]
        isDevNull t =
          case t of
            T_NormalWord _ [T_Literal _ s] -> s == "/dev/null"
            _ -> False
        hasTarget =
          any (\(t, x) -> x /= "" && x `isPrefixOf` "target-directory" && isDevNull t) args

checkHalt = CommandCheck (Basename "halt") $ \t ->
  warn (getId t) 5110 "高危命令检测: 检测到系统关闭命令(halt), 该命令为高危命令请谨慎使用."

checkPoweroff = CommandCheck (Basename "poweroff") $ \t ->
  warn (getId t) 5111 "高危命令检测: 检测到系统关闭命令(poweroff), 该命令为高危命令请谨慎使用."

mkfsCommandList =
  [ "mkfs",
    "mkfs.bfs",
    "mkfs.cramfs",
    "mkfs.ext2",
    "mkfs.ext3",
    "mkfs.ext4",
    "mkfs.fat",
    "mkfs.minix",
    "mkfs.msdos",
    "mkfs.ntfs",
    "mkfs.vfat",
    "mkfs.xfs"
  ]

checkMkfs str = CommandCheck (Basename str) $ \cmd ->
  warn (getId cmd) 5112 $ "高危命令检测: 检测到 " ++ fromMaybe "" (getDeviceName cmd) ++ " 设备格式化命令(" ++ str ++ "), 该命令为高危命令请谨慎使用."
  where
    getDeviceName cmd = do
      opts <- parseOpts $ arguments cmd
      (_, (dev, _)) <- find (null . fst) opts
      getLiteralString dev

    parseOpts tokens = case str of
      "mkfs.bfs" -> getBsdOpts "clhVvN:V:F:" tokens
      "mkfs.cramfs" -> getBsdOpts "vEb:e:N:i:n:pszl" tokens
      "mkfs.fat" -> getBsdOpts "aAb:cCD:f:F:g:h:i:Il:m:M:n:r:R:s:S:v" tokens
      _ | str == "mkfs.xfs" || str == "mkfs.msdos" || str == "mkfs.vfat" -> getBsdOpts "b:m:d:fi:Kl:L:n:Np:qr:s:V" tokens
      _ | "mkfs.ext" `isPrefixOf` str -> getGnuOpts "c:l:b:C:i:I:J:G:N:d:m:o:g:L:M:O:r:E:t:T:U:e:z:jnqvDFKSV" tokens
      _ -> Nothing

checkSystemctlStop = CommandCheck (Basename "systemctl") (f . arguments)
  where
    isOption x = "-" `isPrefixOf` concat (oversimplify x)

    f args = case partition isOption args of
      (_, command : unit : _) ->
        case getLiteralString command of
          Just str
            | str == "stop" || str == "kill" ->
                warn (getId command) 5113 $ "高危命令检测: 检测到停止系统服务 " ++ concat (oversimplify unit) ++ " (systemctl " ++ str ++ "), 该命令为高危命令请谨慎使用"
          Just str
            | str == "restart" || str == "try-restart" ->
                warn (getId command) 5113 $ "高危命令检测: 检测到重启系统服务 " ++ concat (oversimplify unit) ++ " (systemctl " ++ str ++ "), 该命令为高危命令请谨慎使用"
          _ -> return ()
      _ -> return ()

checkServiceStop = CommandCheck (Basename "service") (f . arguments)
  where
    f (service : cmd : _) = case getLiteralString cmd of
      Just "stop" ->
        warn (getId cmd) 5114 $
          "高危命令检测: 检测到停止系统服务 " ++ concat (oversimplify service) ++ " (service .. stop), 该命令为高危命令请谨慎使用"
      Just "restart" ->
        warn (getId cmd) 5114 $
          "高危命令检测: 检测到重启系统服务 " ++ concat (oversimplify service) ++ " (service .. restart), 该命令为高危命令请谨慎使用"
      Just "--full-restart" ->
        warn (getId cmd) 5114 $
          "高危命令检测: 检测到重启系统服务 " ++ concat (oversimplify service) ++ " (service .. --full-restart), 该命令为高危命令请谨慎使用"
      _ -> return ()
    f _ = return ()

checkSSHHost = CommandCheck (Basename "ssh") (f . arguments)
  where
    flagsForSSH = "46AaCfGgKkMNnqsTtVvXxYyB:b:c:D:E:e:F:I:i:J:L:l:m:O:o:P:p:R:S:W:w:"
    f args =
      case getGnuOpts flagsForSSH args >>= find (\(x, _) -> x == "") of
        Just (_, (t, _)) -> check t
        x -> return ()
    check (T_NormalWord id [T_Literal _ user, T_Literal _ "@", T_Literal _ host]) =
      if user == "root"
        then err id 5003 $ "检测到 ssh 命令使用 root 用户登陆节点 " ++ host ++ ", 原则上不允许使用 root 用户登陆节点, 请确认权限."
        else warn id 5003 $ "检测到 ssh 命令使用 " ++ user ++ " 用户登陆节点 " ++ host ++ ", 请确认权限."
    check (T_NormalWord id [T_Literal _ host]) =
      warn id 5003 $ "检测到 ssh 命令使用匿名用户登陆节点 " ++ host ++ ", 请确认权限."
    check x = info (getId x) 2029 "123123"

checkSu = CommandCheck (Basename "su") (f . arguments)
  where
    suString = "mplfPhV"
    suLongOpts =
      [ ("preserve-environment", False),
        ("w", True),
        ("whitelist-environment", True),
        ("g", True),
        ("group", True),
        ("login", False),
        ("c", True),
        ("command", True),
        ("G", True),
        ("supp-group", True),
        ("session-command", True),
        ("s", True),
        ("shell", True)
      ]
    f args = case getOpts (False, True) suString suLongOpts args >>= find (\(x, _) -> x == "") of
      Just (_, (t, _)) -> check t
      Nothing -> return ()

    check (T_NormalWord id [T_Literal _ u]) =
      if u == "root"
        then err id 5004 "检测到切换到 root 用户执行, 原则上不可使用超级管理员用户进行脚本的执行工作, 请确认权限."
        else warn id 5004 $ "检测到切换到用户 " ++ u ++ ",请确认权限"
    check _ = return ()

checkChmod = CommandCheck (Basename "chmod") (f . arguments)
  where
    isOption x = "-" `isPrefixOf` concat (oversimplify x)
    f args = case partition isOption args of
      (_, mode : _) -> check mode
      _ -> return ()

    split char str =
      split' str []
      where
        split' (a : rest) element =
          if a == char
            then reverse element : split' rest []
            else split' rest (a : element)
        split' [] element = [reverse element]

    check mode = case getLiteralString mode of
      Just m ->
        when (any (`elem` ["777", "a+rwx", "+rwx"]) $ split ',' m) $
          warn (getId mode) 5005 $
            "检测到 chmod 命令为文件赋予绝对权限 '" ++ m ++ "' ,请评估权限是否合理."
      Nothing -> return ()

checkSudo = CommandCheck (Basename "sudo") f
  where
    f t =
      warn (getId t) 5008 $
        "检测到使用 sudo 执行命令, 用户为 " ++ fromMaybe "默认" getSudoUser ++ " 命令为 " ++ fromMaybe "" getSudoComand ++ ", 请评估权限是否合理."
      where
        args = arguments t
        getOpts = getBsdOpts "vAknSbEHPa:g:h:p:u:c:T:r:" args
        getSudoUser = do
          opts <- getOpts
          (option, value) <- lookup "u" opts
          getLiteralString value
        getSudoComand = do
          opts <- getOpts
          let cmd = filter (\(option, _) -> option == "") opts
              cmd' = map (\(_, (_, value)) -> fromMaybe "" $ getLiteralString value) cmd
          Just $ unwords cmd'

checkDd = CommandCheck (Basename "dd") f
  where
    isOuputFileOpt = isPrefixOf "of="
    getOutputFile str
      | "of=/dev/sd" `isPrefixOf` str = Just $ drop (length "of=") str
      | otherwise = Nothing

    f t = case find isOuputFileOpt (map (fromMaybe "" . getLiteralString) $ arguments t) >>= getOutputFile of
      Just x -> err (getId t) 5115 $ "高危命令检测: 检测到磁盘写入命令 dd, 可能导致磁盘 " ++ x ++ " 损坏, 请谨慎操作"
      Nothing -> warn (getId t) 5115 "高危命令检测: 检测到磁盘写入命令 dd, 请谨慎操作"

-- checkTimeout = CommandCheck (Basename "timeout") $ \t ->
--     info (getId t) 6001 "识别到使用了 'timeout' 命令, 建议通过 'timeout' 命令来限制执行时间, 在脚本超时情况下终止脚本执行"

-- checkExit = CommandCheck (Exactly "exit") $ \t ->
--     case params t of
--         [T_NormalWord _ [T_Literal _ s]] ->
--             when (s == "0") $
--                 warn (getId t) 6005 "检测到退出命令(exit), 退出码为0, 脚本遇到错误或异常情况主动退出, 应使用非 0 的退出状态码, 可根据脚本的具体执行的操作、功能或步骤定义相应的错误类型"
--         _ -> return ()
--   where
--     params t = [x | (x,"") <- getAllFlags t]

return []

runTests = $([|$(forAllProperties) (quickCheckWithResult (stdArgs {maxSuccess = 1}))|])

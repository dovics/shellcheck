{-# LANGUAGE FlexibleContexts #-}
{-
    Copyright 2012-2020 Vidar Holen

    This file is part of ShellCheck.
    https://www.shellcheck.net

    ShellCheck is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    ShellCheck is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module ShellCheck.Checks.ShellSupport (checker, ShellCheck.Checks.ShellSupport.runTests) where

import Control.Monad
import Control.Monad.RWS
import Data.Char
import Data.Functor.Identity
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import ShellCheck.AST
import ShellCheck.ASTLib
import ShellCheck.AnalyzerLib
import ShellCheck.Interface
import ShellCheck.Message
import ShellCheck.Prelude
import ShellCheck.Regex
import Test.QuickCheck.All (forAllProperties)
import Test.QuickCheck.Test (maxSuccess, quickCheckWithResult, stdArgs)

data ForShell = ForShell [Shell] (Token -> Analysis)

getChecker params list =
  Checker
    { perScript = nullCheck,
      perToken = foldl composeAnalyzers nullCheck $ mapMaybe include list
    }
  where
    shell = shellType params
    include (ForShell list a) = do
      guard $ shell `elem` list
      return a

checker params = getChecker params checks

checks =
  [ checkForDecimals,
    checkBashisms,
    checkEchoSed,
    checkBraceExpansionVars,
    checkMultiDimensionalArrays,
    checkPS1Assignments,
    checkMultipleBangs,
    checkBangAfterPipe
  ]

testChecker (ForShell _ t) =
  Checker
    { perScript = nullCheck,
      perToken = t
    }

verify c s = producesComments (testChecker c) s == Just True

verifyNot c s = producesComments (testChecker c) s == Just False

prop_checkForDecimals1 = verify checkForDecimals "((3.14*c))"

prop_checkForDecimals2 = verify checkForDecimals "foo[1.2]=bar"

prop_checkForDecimals3 = verifyNot checkForDecimals "declare -A foo; foo[1.2]=bar"

checkForDecimals = ForShell [Sh, Dash, BusyboxSh, Bash] f
  where
    f t@(TA_Expansion id _) = sequence_ $ do
      first : rest <- getLiteralString t
      guard $ isDigit first && '.' `elem` rest
      return $ err id 2079 "(( )) doesn't support decimals. Use bc or awk."
    f _ = return ()

prop_checkBashisms = verify checkBashisms "while read a; do :; done < <(a)"

prop_checkBashisms2 = verify checkBashisms "[ foo -nt bar ]"

prop_checkBashisms3 = verify checkBashisms "echo $((i++))"

prop_checkBashisms4 = verify checkBashisms "rm !(*.hs)"

prop_checkBashisms5 = verify checkBashisms "source file"

prop_checkBashisms6 = verify checkBashisms "[ \"$a\" == 42 ]"

prop_checkBashisms6b = verify checkBashisms "test \"$a\" == 42"

prop_checkBashisms6c = verify checkBashisms "[ foo =~ bar ]"

prop_checkBashisms6d = verify checkBashisms "test foo =~ bar"

prop_checkBashisms7 = verify checkBashisms "echo ${var[1]}"

prop_checkBashisms8 = verify checkBashisms "echo ${!var[@]}"

prop_checkBashisms9 = verify checkBashisms "echo ${!var*}"

prop_checkBashisms10 = verify checkBashisms "echo ${var:4:12}"

prop_checkBashisms11 = verifyNot checkBashisms "echo ${var:-4}"

prop_checkBashisms12 = verify checkBashisms "echo ${var//foo/bar}"

prop_checkBashisms13 = verify checkBashisms "exec -c env"

prop_checkBashisms14 = verify checkBashisms "echo -n \"Foo: \""

prop_checkBashisms15 = verify checkBashisms "let n++"

prop_checkBashisms16 = verify checkBashisms "echo $RANDOM"

prop_checkBashisms17 = verify checkBashisms "echo $((RANDOM%6+1))"

prop_checkBashisms18 = verify checkBashisms "foo &> /dev/null"

prop_checkBashisms19 = verify checkBashisms "foo > file*.txt"

prop_checkBashisms20 = verify checkBashisms "read -ra foo"

prop_checkBashisms21 = verify checkBashisms "[ -a foo ]"

prop_checkBashisms21b = verify checkBashisms "test -a foo"

prop_checkBashisms22 = verifyNot checkBashisms "[ foo -a bar ]"

prop_checkBashisms23 = verify checkBashisms "trap mything ERR INT"

prop_checkBashisms24 = verifyNot checkBashisms "trap mything INT TERM"

prop_checkBashisms25 = verify checkBashisms "cat < /dev/tcp/host/123"

prop_checkBashisms26 = verify checkBashisms "trap mything ERR SIGTERM"

prop_checkBashisms27 = verify checkBashisms "echo *[^0-9]*"

prop_checkBashisms28 = verify checkBashisms "exec {n}>&2"

prop_checkBashisms29 = verify checkBashisms "echo ${!var}"

prop_checkBashisms30 = verify checkBashisms "printf -v '%s' \"$1\""

prop_checkBashisms31 = verify checkBashisms "printf '%q' \"$1\""

prop_checkBashisms32 = verifyNot checkBashisms "#!/bin/dash\n[ foo -nt bar ]"

prop_checkBashisms33 = verify checkBashisms "#!/bin/sh\necho -n foo"

prop_checkBashisms34 = verifyNot checkBashisms "#!/bin/dash\necho -n foo"

prop_checkBashisms35 = verifyNot checkBashisms "#!/bin/dash\nlocal foo"

prop_checkBashisms36 = verifyNot checkBashisms "#!/bin/dash\nread -p foo -r bar"

prop_checkBashisms37 = verifyNot checkBashisms "HOSTNAME=foo; echo $HOSTNAME"

prop_checkBashisms38 = verify checkBashisms "RANDOM=9; echo $RANDOM"

prop_checkBashisms39 = verify checkBashisms "foo-bar() { true; }"

prop_checkBashisms40 = verify checkBashisms "echo $(<file)"

prop_checkBashisms41 = verify checkBashisms "echo `<file`"

prop_checkBashisms42 = verify checkBashisms "trap foo int"

prop_checkBashisms43 = verify checkBashisms "trap foo sigint"

prop_checkBashisms44 = verifyNot checkBashisms "#!/bin/dash\ntrap foo int"

prop_checkBashisms45 = verifyNot checkBashisms "#!/bin/dash\ntrap foo INT"

prop_checkBashisms46 = verify checkBashisms "#!/bin/dash\ntrap foo SIGINT"

prop_checkBashisms47 = verify checkBashisms "#!/bin/dash\necho foo 42>/dev/null"

prop_checkBashisms48 = verifyNot checkBashisms "#!/bin/sh\necho $LINENO"

prop_checkBashisms49 = verify checkBashisms "#!/bin/dash\necho $MACHTYPE"

prop_checkBashisms50 = verify checkBashisms "#!/bin/sh\ncmd >& file"

prop_checkBashisms51 = verifyNot checkBashisms "#!/bin/sh\ncmd 2>&1"

prop_checkBashisms52 = verifyNot checkBashisms "#!/bin/sh\ncmd >&2"

prop_checkBashisms52b = verifyNot checkBashisms "#!/bin/sh\ncmd >& $var"

prop_checkBashisms52c = verify checkBashisms "#!/bin/sh\ncmd >& $dir/$var"

prop_checkBashisms53 = verifyNot checkBashisms "#!/bin/sh\nprintf -- -f\n"

prop_checkBashisms54 = verify checkBashisms "#!/bin/sh\nfoo+=bar"

prop_checkBashisms55 = verify checkBashisms "#!/bin/sh\necho ${@%foo}"

prop_checkBashisms56 = verifyNot checkBashisms "#!/bin/sh\necho ${##}"

prop_checkBashisms57 = verifyNot checkBashisms "#!/bin/dash\nulimit -c 0"

prop_checkBashisms58 = verify checkBashisms "#!/bin/sh\nulimit -c 0"

prop_checkBashisms59 = verify checkBashisms "#!/bin/sh\njobs -s"

prop_checkBashisms60 = verifyNot checkBashisms "#!/bin/sh\njobs -p"

prop_checkBashisms61 = verifyNot checkBashisms "#!/bin/sh\njobs -lp"

prop_checkBashisms62 = verify checkBashisms "#!/bin/sh\nexport -f foo"

prop_checkBashisms63 = verifyNot checkBashisms "#!/bin/sh\nexport -p"

prop_checkBashisms64 = verify checkBashisms "#!/bin/sh\nreadonly -a"

prop_checkBashisms65 = verifyNot checkBashisms "#!/bin/sh\nreadonly -p"

prop_checkBashisms66 = verifyNot checkBashisms "#!/bin/sh\ncd -P ."

prop_checkBashisms67 = verify checkBashisms "#!/bin/sh\ncd -P -e ."

prop_checkBashisms68 = verify checkBashisms "#!/bin/sh\numask -p"

prop_checkBashisms69 = verifyNot checkBashisms "#!/bin/sh\numask -S"

prop_checkBashisms70 = verify checkBashisms "#!/bin/sh\ntrap -l"

prop_checkBashisms71 = verify checkBashisms "#!/bin/sh\ntype -a ls"

prop_checkBashisms72 = verifyNot checkBashisms "#!/bin/sh\ntype ls"

prop_checkBashisms73 = verify checkBashisms "#!/bin/sh\nunset -n namevar"

prop_checkBashisms74 = verifyNot checkBashisms "#!/bin/sh\nunset -f namevar"

prop_checkBashisms75 = verifyNot checkBashisms "#!/bin/sh\necho \"-n foo\""

prop_checkBashisms76 = verifyNot checkBashisms "#!/bin/sh\necho \"-ne foo\""

prop_checkBashisms77 = verifyNot checkBashisms "#!/bin/sh\necho -Q foo"

prop_checkBashisms78 = verify checkBashisms "#!/bin/sh\necho -ne foo"

prop_checkBashisms79 = verify checkBashisms "#!/bin/sh\nhash -l"

prop_checkBashisms80 = verifyNot checkBashisms "#!/bin/sh\nhash -r"

prop_checkBashisms81 = verifyNot checkBashisms "#!/bin/dash\nhash -v"

prop_checkBashisms82 = verifyNot checkBashisms "#!/bin/sh\nset -v +o allexport -o errexit -C"

prop_checkBashisms83 = verifyNot checkBashisms "#!/bin/sh\nset --"

prop_checkBashisms84 = verify checkBashisms "#!/bin/sh\nset -o pipefail"

prop_checkBashisms85 = verify checkBashisms "#!/bin/sh\nset -B"

prop_checkBashisms86 = verifyNot checkBashisms "#!/bin/dash\nset -o emacs"

prop_checkBashisms87 = verify checkBashisms "#!/bin/sh\nset -o emacs"

prop_checkBashisms88 = verifyNot checkBashisms "#!/bin/sh\nset -- wget -o foo 'https://some.url'"

prop_checkBashisms89 = verifyNot checkBashisms "#!/bin/sh\nopts=$-\nset -\"$opts\""

prop_checkBashisms90 = verifyNot checkBashisms "#!/bin/sh\nset -o \"$opt\""

prop_checkBashisms91 = verify checkBashisms "#!/bin/sh\nwait -n"

prop_checkBashisms92 = verify checkBashisms "#!/bin/sh\necho $((16#FF))"

prop_checkBashisms93 = verify checkBashisms "#!/bin/sh\necho $(( 10#$(date +%m) ))"

prop_checkBashisms94 = verify checkBashisms "#!/bin/sh\n[ -v var ]"

prop_checkBashisms95 = verify checkBashisms "#!/bin/sh\necho $_"

prop_checkBashisms96 = verifyNot checkBashisms "#!/bin/dash\necho $_"

prop_checkBashisms97 = verify checkBashisms "#!/bin/sh\necho ${var,}"

prop_checkBashisms98 = verify checkBashisms "#!/bin/sh\necho ${var^^}"

prop_checkBashisms99 = verify checkBashisms "#!/bin/dash\necho [^f]oo"

prop_checkBashisms100 = verify checkBashisms "read -r"

prop_checkBashisms101 = verify checkBashisms "read"

prop_checkBashisms102 = verifyNot checkBashisms "read -r foo"

prop_checkBashisms103 = verifyNot checkBashisms "read foo"

prop_checkBashisms104 = verifyNot checkBashisms "read ''"

prop_checkBashisms105 = verifyNot checkBashisms "#!/bin/busybox sh\nset -o pipefail"

prop_checkBashisms106 = verifyNot checkBashisms "#!/bin/busybox sh\nx=x\n[[ \"$x\" = \"$x\" ]]"

prop_checkBashisms107 = verifyNot checkBashisms "#!/bin/busybox sh\nx=x\n[ \"$x\" == \"$x\" ]"

prop_checkBashisms108 = verifyNot checkBashisms "#!/bin/busybox sh\necho magic &> /dev/null"

prop_checkBashisms109 = verifyNot checkBashisms "#!/bin/busybox sh\ntrap stop EXIT SIGTERM"

prop_checkBashisms110 = verifyNot checkBashisms "#!/bin/busybox sh\nsource /dev/null"

prop_checkBashisms111 = verify checkBashisms "#!/bin/dash\nx='test'\n${x:0:3}" -- SC3057

prop_checkBashisms112 = verifyNot checkBashisms "#!/bin/busybox sh\nx='test'\n${x:0:3}" -- SC3057

prop_checkBashisms113 = verify checkBashisms "#!/bin/dash\nx='test'\n${x/st/xt}" -- SC3060

prop_checkBashisms114 = verifyNot checkBashisms "#!/bin/busybox sh\nx='test'\n${x/st/xt}" -- SC3060

prop_checkBashisms115 = verify checkBashisms "#!/bin/busybox sh\nx='test'\n${!x}" -- SC3053

prop_checkBashisms116 = verify checkBashisms "#!/bin/busybox sh\nx='test'\n${x[1]}" -- SC3054

prop_checkBashisms117 = verify checkBashisms "#!/bin/busybox sh\nx='test'\n${!x[@]}" -- SC3055

prop_checkBashisms118 = verify checkBashisms "#!/bin/busybox sh\nxyz=1\n${!x*}" -- SC3056

prop_checkBashisms119 = verify checkBashisms "#!/bin/busybox sh\nx='test'\n${x^^[t]}" -- SC3059

prop_checkBashisms120 = verify checkBashisms "#!/bin/sh\n[ x == y ]"

prop_checkBashisms121 = verifyNot checkBashisms "#!/bin/sh\n# shellcheck shell=busybox\n[ x == y ]"

prop_checkBashisms122 = verify checkBashisms "#!/bin/dash\n$'a'"

prop_checkBashisms123 = verifyNot checkBashisms "#!/bin/busybox sh\n$'a'"

prop_checkBashisms124 = verify checkBashisms "#!/bin/dash\ntype -p test"

prop_checkBashisms125 = verifyNot checkBashisms "#!/bin/busybox sh\ntype -p test"

prop_checkBashisms126 = verifyNot checkBashisms "#!/bin/busybox sh\nread -p foo -r bar"

prop_checkBashisms127 = verifyNot checkBashisms "#!/bin/busybox sh\necho -ne foo"

checkBashisms = ForShell [Sh, Dash, BusyboxSh] $ \t -> do
  params <- ask
  kludge params t
  where
    -- This code was copy-pasted from Analytics where params was a variable
    kludge params = bashism
      where
        isBusyboxSh = shellType params == BusyboxSh
        isDash = shellType params == Dash || isBusyboxSh
        warnMsg id code s =
          if isDash
            then err id code $ warpShellSupportMessage Dash s
            else warn id code $ warpShellSupportMessage Sh s
        asStr = getLiteralString

        bashism (T_ProcSub id _ _) = warnMsg id 3001 $ getMessage 3001 []
        bashism (T_Extglob id _ _) = warnMsg id 3002 $ getMessage 3002 []
        bashism (T_DollarSingleQuoted id _) =
          unless isBusyboxSh $ warnMsg id 3003 $ getMessage 3003 []
        bashism (T_DollarDoubleQuoted id _) = warnMsg id 3004 $ getMessage 3004 []
        bashism (T_ForArithmetic id _ _ _ _) = warnMsg id 3005 $ getMessage 3005 []
        bashism (T_Arithmetic id _) = warnMsg id 3006 $ getMessage 3006 []
        bashism (T_DollarBracket id _) = warnMsg id 3007 $ getMessage 3007 []
        bashism (T_SelectIn id _ _ _) = warnMsg id 3008 $ getMessage 3008 []
        bashism (T_BraceExpansion id _) = warnMsg id 3009 $ getMessage 3009 []
        bashism (T_Condition id DoubleBracket _) =
          unless isBusyboxSh $ warnMsg id 3010 $ getMessage 3010 []
        bashism (T_HereString id _) = warnMsg id 3011 $ getMessage 3011 []
        bashism (TC_Binary id SingleBracket op _ _)
          | op `elem` ["<", ">", "\\<", "\\>", "<=", ">=", "\\<=", "\\>="] =
            unless isDash $ warnMsg id 3012 $ getMessage 3012 [op]
        bashism (T_SimpleCommand id _ [asStr -> Just "test", lhs, asStr -> Just op, rhs])
          | op `elem` ["<", ">", "\\<", "\\>", "<=", ">=", "\\<=", "\\>="] =
            unless isDash $ warnMsg id 3012 $ getMessage 3012 [op]
        bashism (TC_Binary id SingleBracket op _ _)
          | op `elem` ["-ot", "-nt", "-ef"] =
            unless isDash $ warnMsg id 3013 $ getMessage 3013 [op]
        bashism (T_SimpleCommand id _ [asStr -> Just "test", lhs, asStr -> Just op, rhs])
          | op `elem` ["-ot", "-nt", "-ef"] =
            unless isDash $ warnMsg id 3013 $ getMessage 3013 [op]
        bashism (TC_Binary id SingleBracket "==" _ _) =
          unless isBusyboxSh $ warnMsg id 3014 $ getMessage 3014 []
        bashism (T_SimpleCommand id _ [asStr -> Just "test", lhs, asStr -> Just "==", rhs]) =
          unless isBusyboxSh $ warnMsg id 3014 $ getMessage 3014 []
        bashism (TC_Binary id SingleBracket "=~" _ _) =
          warnMsg id 3015 $ getMessage 3015 []
        bashism (T_SimpleCommand id _ [asStr -> Just "test", lhs, asStr -> Just "=~", rhs]) =
          warnMsg id 3015 $ getMessage 3015 []
        bashism (TC_Unary id SingleBracket "-v" _) =
          warnMsg id 3016 $ getMessage 3016 []
        bashism (T_SimpleCommand id _ [asStr -> Just "test", asStr -> Just "-v", _]) =
          warnMsg id 3016 $ getMessage 3016 []
        bashism (TC_Unary id _ "-a" _) =
          warnMsg id 3017 $ getMessage 3017 []
        bashism (T_SimpleCommand id _ [asStr -> Just "test", asStr -> Just "-a", _]) =
          warnMsg id 3017 $ getMessage 3017 []
        bashism (TA_Unary id op _)
          | op `elem` ["|++", "|--", "++|", "--|"] =
            warnMsg id 3018 $ getMessage 3018 [filter (/= '|') op]
        bashism (TA_Binary id "**" _ _) = warnMsg id 3019 $ getMessage 3019 []
        bashism (T_FdRedirect id "&" (T_IoFile _ (T_Greater _) _)) =
          unless isBusyboxSh $ warnMsg id 3020 $ getMessage 3020 []
        bashism (T_FdRedirect id "" (T_IoFile _ (T_GREATAND _) file)) =
          unless (all isDigit $ onlyLiteralString file) $ warnMsg id 3021 $ getMessage 3021 []
        bashism (T_FdRedirect id ('{' : _) _) = warnMsg id 3022 $ getMessage 3022 []
        bashism (T_FdRedirect id num _)
          | all isDigit num && length num > 1 = warnMsg id 3023 $ getMessage 3023 []
        bashism (T_Assignment id Append _ _ _) =
          warnMsg id 3024 $ getMessage 3024 []
        bashism (T_IoFile id _ word)
          | isNetworked =
            warnMsg id 3025 $ getMessage 3025 []
          where
            file = onlyLiteralString word
            isNetworked = any (`isPrefixOf` file) ["/dev/tcp", "/dev/udp"]
        bashism (T_Glob id str)
          | "[^" `isInfixOf` str =
            warnMsg id 3026 $ getMessage 3026 []
        bashism t@(TA_Variable id str _)
          | isBashVariable str =
            warnMsg id 3028 $ getMessage 3028 [str]
        bashism t@(T_DollarBraced id _ token) = do
          unless isBusyboxSh $ mapM_ check simpleExpansions
          mapM_ check advancedExpansions
          when (isBashVariable var) $
            warnMsg id 3028 $ getMessage 3028 [var]
          where
            str = concat $ oversimplify token
            var = getBracedReference str
            check (regex, code, feature) =
              when (isJust $ matchRegex regex str) $ warnMsg id code feature
        bashism t@(T_Pipe id "|&") =
          warnMsg id 3029 $ getMessage 3029 []
        bashism (T_Array id _) =
          warnMsg id 3030 $ getMessage 3030 []
        bashism (T_IoFile id _ t)
          | isGlob t =
            warnMsg id 3031 $ getMessage 3031 []
        bashism (T_CoProc id _ _) =
          warnMsg id 3032 $ getMessage 3032 []
        bashism (T_Function id _ _ str _)
          | not (isVariableName str) =
            warnMsg id 3033 $ getMessage 3033 []
        bashism (T_DollarExpansion id [x])
          | isOnlyRedirection x =
            warnMsg id 3034 $ getMessage 3034 []
        bashism (T_Backticked id [x])
          | isOnlyRedirection x =
            warnMsg id 3035 $ getMessage 3035 []
        bashism t@(T_SimpleCommand _ _ (cmd : arg : _))
          | t `isCommand` "echo" && argString `matches` flagRegex =
            if isBusyboxSh
              then
                when (not (argString `matches` busyboxFlagRegex)) $
                  warnMsg (getId arg) 3036 $ getMessage 3036 ["-n", "-e"]
              else
                if isDash
                  then
                    when (argString /= "-n") $
                      warnMsg (getId arg) 3036 $ getMessage 3036 ["-n"]
                  else warnMsg (getId arg) 3037 $ getMessage 3037 []
          where
            argString = concat $ oversimplify arg
            flagRegex = mkRegex "^-[eEsn]+$"
            busyboxFlagRegex = mkRegex "^-[en]+$"
        bashism t@(T_SimpleCommand _ _ (cmd : arg : _))
          | getLiteralString cmd == Just "exec" && "-" `isPrefixOf` concat (oversimplify arg) =
            warnMsg (getId arg) 3038 $ getMessage 3038 []
        bashism t@(T_SimpleCommand id _ _)
          | t `isCommand` "let" = warnMsg id 3039 $ getMessage 3039 []
        bashism t@(T_SimpleCommand _ _ (cmd : args))
          | t `isCommand` "set" =
            unless isDash $
              checkOptions $ getLiteralArgs args
          where
            -- Get the literal options from a list of arguments,
            -- up until the first non-literal one
            getLiteralArgs :: [Token] -> [(Id, String)]
            getLiteralArgs = foldr go []
              where
                go first rest = case getLiteralString first of
                  Just str -> (getId first, str) : rest
                  Nothing -> []

            -- Check a flag-option pair (such as -o errexit)
            checkOptions (flag@(fid, flag') : opt@(oid, opt') : rest)
              | flag' `matches` oFlagRegex = do
                when (opt' `notElem` longOptions) $
                  warnMsg oid 3040 $ getMessage 3040 [opt']
                checkFlags (flag : rest)
              | otherwise = checkFlags (flag : opt : rest)
            checkOptions (flag : rest) = checkFlags (flag : rest)
            checkOptions _ = return ()

            -- Check that each option in a sequence of flags
            -- (such as -aveo) is valid
            checkFlags (flag@(fid, flag') : rest)
              | startsOption flag' = do
                unless (flag' `matches` validFlagsRegex) $
                  forM_ (tail flag') $ \letter ->
                    when (letter `notElem` optionsSet) $
                      warnMsg fid 3041 $ getMessage 3041 ['-' : letter : " is"]
                checkOptions rest
              | beginsWithDoubleDash flag' = do
                warnMsg fid 3042 $ getMessage 3042 [flag']
                checkOptions rest
              -- Either a word that doesn't start with a dash, or simply '--',
              -- so stop checking.
              | otherwise = return ()
            checkFlags [] = return ()

            options = "abCefhmnuvxo"
            optionsSet = Set.fromList options
            startsOption = (`matches` mkRegex "^(\\+|-[^-])")
            oFlagRegex = mkRegex $ "^[-+][" <> options <> "]*o$"
            validFlagsRegex = mkRegex $ "^[-+]([" <> options <> "]+o?|o)$"
            beginsWithDoubleDash = (`matches` mkRegex "^--.+$")
            longOptions =
              Set.fromList
                [ "allexport",
                  "errexit",
                  "ignoreeof",
                  "monitor",
                  "noclobber",
                  "noexec",
                  "noglob",
                  "nolog",
                  "notify",
                  "nounset",
                  "verbose",
                  "vi",
                  "xtrace"
                ]
        bashism t@(T_SimpleCommand id _ (cmd : rest)) =
          let name = fromMaybe "" $ getCommandName t
              flags = getLeadingFlags t
           in do
                when (name == "local" && not isDash) $
                  -- This is so commonly accepted that we'll make it a special case
                  warnMsg id 3043 $ getMessage 3043 []
                when (name `elem` unsupportedCommands) $
                  warnMsg id 3044 $ getMessage 3044 [name]
                sequence_ $ do
                  allowed' <- Map.lookup name allowedFlags
                  allowed <- allowed'
                  (word, flag) <-
                    find
                      (\x -> (not . null . snd $ x) && snd x `notElem` allowed)
                      flags
                  return . warnMsg (getId word) 3045 $ getMessage 3045 [name, flag]

                when (name == "source" && not isBusyboxSh) $
                  warnMsg id 3046 $ getMessage 3046 []
                when (name == "trap") $
                  let check token = sequence_ $ do
                        str <- getLiteralString token
                        let upper = map toUpper str
                        return $ do
                          when (upper `elem` ["ERR", "DEBUG", "RETURN"]) $
                            warnMsg (getId token) 3047 $ getMessage 3047 [str]
                          when (not isBusyboxSh && "SIG" `isPrefixOf` upper) $
                            warnMsg (getId token) 3048 $ getMessage 3048 []
                          when (not isDash && upper /= str) $
                            warnMsg (getId token) 3049 $ getMessage 3049 []
                   in mapM_ check (drop 1 rest)

                when (name == "printf") $
                  sequence_ $ do
                    format <- rest !!! 0 -- flags are covered by allowedFlags
                    let literal = onlyLiteralString format
                    guard $ "%q" `isInfixOf` literal
                    return $ warnMsg (getId format) 3050 $ getMessage 3050 []

                when (name == "read" && all isFlag rest) $
                  warnMsg (getId cmd) 3061 $ getMessage 3061 []
          where
            unsupportedCommands =
              [ "let",
                "caller",
                "builtin",
                "complete",
                "compgen",
                "declare",
                "dirs",
                "disown",
                "enable",
                "mapfile",
                "readarray",
                "pushd",
                "popd",
                "shopt",
                "suspend",
                "typeset"
              ]
            allowedFlags =
              Map.fromList
                [ ("cd", Just ["L", "P"]),
                  ("exec", Just []),
                  ("export", Just ["p"]),
                  ("hash", Just $ if isDash then ["r", "v"] else ["r"]),
                  ("jobs", Just ["l", "p"]),
                  ("printf", Just []),
                  ("read", Just $ if isDash || isBusyboxSh then ["r", "p"] else ["r"]),
                  ("readonly", Just ["p"]),
                  ("trap", Just []),
                  ("type", Just $ if isBusyboxSh then ["p"] else []),
                  ("ulimit", if isDash then Nothing else Just ["f"]),
                  ("umask", Just ["S"]),
                  ("unset", Just ["f", "v"]),
                  ("wait", Just [])
                ]
        bashism t@(T_SourceCommand id src _)
          | getCommandName src == Just "source" =
            unless isBusyboxSh $
              warnMsg id 3051 $ getMessage 3051 []
        bashism (TA_Expansion _ (T_Literal id str : _))
          | str `matches` radix = warnMsg id 3052 $ getMessage 3052 []
          where
            radix = mkRegex "^[0-9]+#"
        bashism _ = return ()

        varChars = "_0-9a-zA-Z"
        advancedExpansions =
          let re = mkRegex
           in [ (re $ "^![" ++ varChars ++ "]", 3053, getMessage 3053 []),
                (re $ "^[" ++ varChars ++ "]+\\[.*\\]$", 3054, getMessage 3054 []),
                (re $ "^![" ++ varChars ++ "]+\\[[*@]]$", 3055, getMessage 3055 []),
                (re $ "^![" ++ varChars ++ "]+[*@]$", 3056, getMessage 3056 []),
                (re $ "^[" ++ varChars ++ "*@]+(\\[.*\\])?[,^]", 3059, getMessage 3059 [])
              ]
        simpleExpansions =
          let re = mkRegex
           in [ (re $ "^[" ++ varChars ++ "*@]+:[^-=?+]", 3057, getMessage 3057 []),
                (re $ "^([*@][%#]|#[@*])", 3058, getMessage 3058 []),
                (re $ "^[" ++ varChars ++ "*@]+(\\[.*\\])?/", 3060, getMessage 3060 [])
              ]
        bashVars =
          [ -- This list deliberately excludes $BASH_VERSION as it's often used
            -- for shell identification.
            "OSTYPE",
            "MACHTYPE",
            "HOSTTYPE",
            "HOSTNAME",
            "DIRSTACK",
            "EUID",
            "UID",
            "SHLVL",
            "PIPESTATUS",
            "SHELLOPTS",
            "_",
            "BASHOPTS",
            "BASHPID",
            "BASH_ALIASES",
            "BASH_ARGC",
            "BASH_ARGV",
            "BASH_ARGV0",
            "BASH_CMDS",
            "BASH_COMMAND",
            "BASH_EXECUTION_STRING",
            "BASH_LINENO",
            "BASH_REMATCH",
            "BASH_SOURCE",
            "BASH_SUBSHELL",
            "BASH_VERSINFO",
            "EPOCHREALTIME",
            "EPOCHSECONDS",
            "FUNCNAME",
            "GROUPS",
            "MACHTYPE",
            "MAPFILE"
          ]
        bashDynamicVars = ["RANDOM", "SECONDS"]
        dashVars = ["_"]
        isBashVariable var =
          ( var `elem` bashDynamicVars
              || var `elem` bashVars && not (isAssigned var)
          )
            && not (isDash && var `elem` dashVars)
        isAssigned var = any f (variableFlow params)
          where
            f x = case x of
              Assignment (_, _, name, _) -> name == var
              _ -> False

prop_checkEchoSed1 = verify checkEchoSed "FOO=$(echo \"$cow\" | sed 's/foo/bar/g')"

prop_checkEchoSed1b = verify checkEchoSed "FOO=$(sed 's/foo/bar/g' <<< \"$cow\")"

prop_checkEchoSed2 = verify checkEchoSed "rm $(echo $cow | sed -e 's,foo,bar,')"

prop_checkEchoSed2b = verify checkEchoSed "rm $(sed -e 's,foo,bar,' <<< $cow)"

checkEchoSed = ForShell [Bash, Ksh] f
  where
    f (T_Redirecting id lefts r) =
      when (any redirectHereString lefts) $
        checkSed id rcmd
      where
        redirectHereString :: Token -> Bool
        redirectHereString t = case t of
          (T_FdRedirect _ _ T_HereString {}) -> True
          _ -> False
        rcmd = oversimplify r
    f (T_Pipeline id _ [a, b]) =
      when (acmd == ["echo", "${VAR}"]) $
        checkSed id bcmd
      where
        acmd = oversimplify a
        bcmd = oversimplify b
    f _ = return ()

    checkSed id ["sed", v] = checkIn id v
    checkSed id ["sed", "-e", v] = checkIn id v
    checkSed _ _ = return ()

    -- This should have used backreferences, but TDFA doesn't support them
    sedRe = mkRegex "^s(.)([^\n]*)g?$"
    isSimpleSed s = isJust $ do
      [h : _, rest] <- matchRegex sedRe s
      let delimiters = filter (== h) rest
      guard $ length delimiters == 2
    checkIn id s =
      when (isSimpleSed s) $
        style id 2001 $ getMessage 2001 []

prop_checkBraceExpansionVars1 = verify checkBraceExpansionVars "echo {1..$n}"

prop_checkBraceExpansionVars2 = verifyNot checkBraceExpansionVars "echo {1,3,$n}"

prop_checkBraceExpansionVars3 = verify checkBraceExpansionVars "eval echo DSC{0001..$n}.jpg"

prop_checkBraceExpansionVars4 = verify checkBraceExpansionVars "echo {$i..100}"

checkBraceExpansionVars = ForShell [Bash] f
  where
    f t@(T_BraceExpansion id list) = mapM_ check list
      where
        check element =
          when (any (`isInfixOf` toString element) ["$..", "..$"]) $ do
            c <- isEvaled element
            if c
              then style id 2175 $ getMessage 2175 []
              else warn id 2051 $ getMessage 2051 []
    f _ = return ()

    literalExt t =
      case t of
        T_DollarBraced {} -> return "$"
        T_DollarExpansion {} -> return "$"
        T_DollarArithmetic {} -> return "$"
        _ -> return "-"
    toString t = runIdentity $ getLiteralStringExt literalExt t
    isEvaled t = do
      cmd <- getClosestCommandM t
      return $ maybe False (`isUnqualifiedCommand` "eval") cmd

prop_checkMultiDimensionalArrays1 = verify checkMultiDimensionalArrays "foo[a][b]=3"

prop_checkMultiDimensionalArrays2 = verifyNot checkMultiDimensionalArrays "foo[a]=3"

prop_checkMultiDimensionalArrays3 = verify checkMultiDimensionalArrays "foo=( [a][b]=c )"

prop_checkMultiDimensionalArrays4 = verifyNot checkMultiDimensionalArrays "foo=( [a]=c )"

prop_checkMultiDimensionalArrays5 = verify checkMultiDimensionalArrays "echo ${foo[bar][baz]}"

prop_checkMultiDimensionalArrays6 = verifyNot checkMultiDimensionalArrays "echo ${foo[bar]}"

checkMultiDimensionalArrays = ForShell [Bash] f
  where
    f token =
      case token of
        T_Assignment _ _ name (first : second : _) _ -> about second
        T_IndexedElement _ (first : second : _) _ -> about second
        T_DollarBraced _ _ l ->
          when (isMultiDim l) $ about token
        _ -> return ()
    about t = warn (getId t) 2180 $ getMessage 2180 []

    re = mkRegex "^\\[.*\\]\\[.*\\]" -- Fixme, this matches ${foo:- [][]} and such as well
    isMultiDim l = getBracedModifier (concat $ oversimplify l) `matches` re

prop_checkPS11 = verify checkPS1Assignments "PS1='\\033[1;35m\\$ '"

prop_checkPS11a = verify checkPS1Assignments "export PS1='\\033[1;35m\\$ '"

prop_checkPSf2 = verify checkPS1Assignments "PS1='\\h \\e[0m\\$ '"

prop_checkPS13 = verify checkPS1Assignments "PS1=$'\\x1b[c '"

prop_checkPS14 = verify checkPS1Assignments "PS1=$'\\e[3m; '"

prop_checkPS14a = verify checkPS1Assignments "export PS1=$'\\e[3m; '"

prop_checkPS15 = verifyNot checkPS1Assignments "PS1='\\[\\033[1;35m\\]\\$ '"

prop_checkPS16 = verifyNot checkPS1Assignments "PS1='\\[\\e1m\\e[1m\\]\\$ '"

prop_checkPS17 = verifyNot checkPS1Assignments "PS1='e033x1B'"

prop_checkPS18 = verifyNot checkPS1Assignments "PS1='\\[\\e\\]'"

checkPS1Assignments = ForShell [Bash] f
  where
    f token = case token of
      (T_Assignment _ _ "PS1" _ word) -> warnFor word
      _ -> return ()

    warnFor word =
      let contents = concat $ oversimplify word
       in when (containsUnescaped contents) $
            info (getId word) 2025 "Make sure all escape sequences are enclosed in \\[..\\] to prevent line wrapping issues"
    containsUnescaped s =
      let unenclosed = subRegex enclosedRegex s ""
       in isJust $ matchRegex escapeRegex unenclosed
    enclosedRegex = mkRegex "\\\\\\[.*\\\\\\]" -- FIXME: shouldn't be eager
    escapeRegex = mkRegex "\\\\x1[Bb]|\\\\e|\x1B|\\\\033"

prop_checkMultipleBangs1 = verify checkMultipleBangs "! ! true"

prop_checkMultipleBangs2 = verifyNot checkMultipleBangs "! true"

checkMultipleBangs = ForShell [Dash, BusyboxSh, Sh] f
  where
    f token = case token of
      T_Banged id (T_Banged _ _) ->
        err id 2325 $ getMessage 2325 []
      _ -> return ()

prop_checkBangAfterPipe1 = verify checkBangAfterPipe "true | ! true"

prop_checkBangAfterPipe2 = verifyNot checkBangAfterPipe "true | ( ! true )"

prop_checkBangAfterPipe3 = verifyNot checkBangAfterPipe "! ! true | true"

checkBangAfterPipe = ForShell [Dash, BusyboxSh, Sh, Bash] f
  where
    f token = case token of
      T_Pipeline _ _ cmds -> mapM_ check cmds
      _ -> return ()

    check token = case token of
      T_Banged id _ ->
        err id 2326 $ getMessage 2326 []
      _ -> return ()

return []

runTests = $([|$(forAllProperties) (quickCheckWithResult (stdArgs {maxSuccess = 1}))|])

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant case" #-}
module ShellCheck.Message (getMessage, warpShellSupportMessage) where

import ShellCheck.Interface (Code, Shell (Dash))

import Control.Monad
import Data.Aeson.Encoding (value)

-- TODO: read language from config file
getMessage code = getLangMessage code "zh"

warpShellSupportMessage shell = warpShellSupportLangMessage shell "zh"

getLangMessage :: Code -> String -> [String] -> String
getLangMessage 1001 lang [next] =
    case lang of
        "zh" -> "在此上下文中 \\" ++ next ++ " 只是常规的 " ++ next ++ "." 
        _    -> "This \\" ++ next ++ " will be a regular '" ++ next ++ "' in this context."

getLangMessage 1003 lang [] =
    case lang of
        "zh" -> "想要转义单引号吗? echo 'This is how it'\\''s done'." 
        _    -> "Want to escape a single quote? echo 'This is how it'\\''s done'."

getLangMessage 1004 lang [] =
    case lang of
        "zh" -> "由于信噪比较低, 检查项在v0.7.2后停用" 
        _    -> "This warning was retired after v0.7.2 due to low signal-to-noise ratio"

getLangMessage 1007 lang [] =
    case lang of
        _    -> "Remove space after = if trying to assign a value (for empty string, use var='' ... )."

getLangMessage 1008 lang [] =
    case lang of
        _    -> "This shebang was unrecognized. ShellCheck only supports sh/bash/dash/ksh/'busybox sh'. Add a 'shell' directive to specify."

getLangMessage 1009 lang [str] =
    case lang of
        _    -> "The mentioned syntax error was in this " ++ str ++ "."

getLangMessage 1010 lang [x] =
    case lang of
        _    -> "Use semicolon or linefeed before '" ++ x ++ "' (or quote to make it literal)."

getLangMessage 1011 lang [] =
    case lang of
        _    -> "This apostrophe terminated the single quoted string!"

getLangMessage 1012 lang [next, name, alternative] =
    case lang of
        _    -> "\\" ++ next ++ " is just literal '" ++ next ++ "' here. For " ++ name ++ ", use " ++ alternative ++ " instead."

getLangMessage 1014 lang [] =
    case lang of
        _    -> "Use 'if cmd; then ..' to check exit code, or 'if [[ $(cmd) == .. ]]' to check output."

getLangMessage 1017 lang [] =
    case lang of
        _    -> "Literal carriage return. Run script through tr -d '\\r' ."

getLangMessage 1018 lang [name] =
    case lang of
        _    -> "This is a " ++ name ++ ". Delete and retype it."

getLangMessage 1019 lang [] =
    case lang of
        _    -> "Expected this to be an argument to the unary condition."

getLangMessage 1020 lang [symbol] =
    case lang of
        _    -> "You need a space before the " ++ symbol ++ "."

getLangMessage 1026 lang [now, suggest] =
    case lang of
        _    -> "If grouping expressions inside " ++ now ++", use " ++ suggest ++"."

getLangMessage 1027 lang [] =
    case lang of
        _    -> "Expected another argument for this operator."

getLangMessage 1028 lang [] =
    case lang of
        _    -> "In [..] you have to escape \\( \\) or preferably combine [..] expressions."

getLangMessage 1029 lang [] =
    case lang of
        _    -> "In [[..]] you shouldn't escape ( or )."

getLangMessage 1033 lang [] =
    case lang of
        _    -> "Test expression was opened with double [[ but closed with single ]. Make sure they match."

getLangMessage 1034 lang [] =
    case lang of
        _    -> "Test expression was opened with single [ but closed with double ]]. Make sure they match."

getLangMessage 1035 lang [position] =
    case lang of
        _    -> "You are missing a required space " ++ position ++ "."

getLangMessage 1036 lang [] =
    case lang of
        _    -> "'(' is invalid here. Did you forget to escape it?"

getLangMessage 1037 lang [] =
    case lang of
        _    -> "Braces are required for positionals over 9, e.g. ${10}."

getLangMessage 1038 lang [sp] =
    case lang of
        _    -> "Shells are space sensitive. Use '< <(cmd)', not '<<" ++ sp ++ "(cmd)'."

getLangMessage 1039 lang [] =
    case lang of
        _    -> "Remove indentation before end token (or use <<- and indent with tabs)."

getLangMessage 1040 lang [] =
    case lang of
        _    -> "When using <<-, you can only indent with tabs."

getLangMessage 1041 lang [message] =
    case lang of
        _    -> "Found '" ++ message ++ "' further down, but not on a separate line."

getLangMessage 1043 lang [message] =
    case lang of
        _    -> "Found " ++ message ++ " further down, but with wrong casing."

getLangMessage 1044 lang [message] =
    case lang of
        _    -> "Couldn't find end token `" ++ message ++ "' in the here document."

getLangMessage 1045 lang [] =
    case lang of
        _    -> "It's not 'foo &; bar', just 'foo & bar'."

getLangMessage 1046 lang [] =
    case lang of
        _    -> "Couldn't find 'fi' for this 'if'."

getLangMessage 1047 lang [] =
    case lang of
        _    -> "Expected 'fi' matching previously mentioned 'if'."

getLangMessage 1048 lang [s] =
    case lang of
        _    -> "Can't have empty " ++ s ++ " clauses (use 'true' as a no-op)."

getLangMessage 1049 lang [symbol] =
    case lang of
        _    -> "Did you forget the 'then' for this '" ++ symbol ++ "'?"

getLangMessage 1050 lang [] =
    case lang of
        _    -> "Expected 'then'."

getLangMessage 1051 lang [] =
    case lang of
        _    -> "Semicolons directly after 'then' are not allowed. Just remove it."

getLangMessage 1052 lang [] =
    case lang of
        _    -> "Semicolons directly after 'then' are not allowed. Just remove it."

getLangMessage 1053 lang [] =
    case lang of
        _    -> "Semicolons directly after 'else' are not allowed. Just remove it."

getLangMessage 1054 lang [] =
    case lang of
        _    -> "You need a space after the '{'."

getLangMessage 1055 lang [] =
    case lang of
        _    -> "You need at least one command here. Use 'true;' as a no-op."

getLangMessage 1056 lang [] =
    case lang of
        _    -> "Expected a '}'. If you have one, try a ; or \\n in front of it."

getLangMessage 1057 lang [] =
    case lang of
        _    -> "Did you forget the 'do' for this loop?"

getLangMessage 1058 lang [] =
    case lang of
        _    -> "Expected 'do'."

getLangMessage 1059 lang [] =
    case lang of
        _    -> "Semicolon is not allowed directly after 'do'. You can just delete it."

getLangMessage 1060 lang [] =
    case lang of
        _    -> "Can't have empty do clauses (use 'true' as a no-op)."

getLangMessage 1061 lang [] =
    case lang of
        _    -> "Couldn't find 'done' for this 'do'."

getLangMessage 1062 lang [] =
    case lang of
        _    -> "Expected 'done' matching previously mentioned 'do'."

getLangMessage 1063 lang [] =
    case lang of
        _    -> "You need a line feed or semicolon before the 'do'."

getLangMessage 1064 lang [] =
    case lang of
        _    -> "Expected a { to open the function definition."

getLangMessage 1065 lang [] =
    case lang of
        _    -> "Trying to declare parameters? Don't. Use () and refer to params as $1, $2.."

getLangMessage 1066 lang [] =
    case lang of
        _    -> "Don't use $ on the left side of assignments."

getLangMessage 1067 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 1068 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 1069 lang [] =
    case lang of
        _    -> "You need a space before the [ ."

getLangMessage 1070 lang [] =
    case lang of
        _    -> "Parsing stopped here. Mismatched keywords or invalid parentheses?"

getLangMessage 1071 lang [] =
    case lang of
        _    -> "ShellCheck only supports sh/bash/dash/ksh/'busybox sh' scripts. Sorry!"

getLangMessage 1072 lang [message] =
    case lang of
        _    -> message ++ " Fix any mentioned problems and try again."

getLangMessage 1073 lang [str] =
    case lang of
        _    -> "Couldn't parse this " ++ str ++ ". Fix to allow more checks."

getLangMessage 1074 lang [] =
    case lang of
        _    -> "Did you forget the ;; after the previous case item?"

getLangMessage 1075 lang [] =
    case lang of
        _    -> "Use 'elif' instead of 'else if' (or put 'if' on new line if nesting)."

getLangMessage 1076 lang [example] =
    case lang of
        _    -> "Trying to do math? Use e.g. " ++ example ++ "."

getLangMessage 1077 lang [] =
    case lang of
        _    -> "For command expansion, the tick should slant left (` vs ´). Use $(..) instead."

getLangMessage 1078 lang [name] =
    case lang of
        _    -> "Did you forget to close this " ++ name ++ "?"

getLangMessage 1079 lang [] =
    case lang of
        _    -> "This is actually an end quote, but due to next char it looks suspect."

getLangMessage 1080 lang [] =
    case lang of
        _    -> "When breaking lines in [ ], you need \\ before the linefeed."

getLangMessage 1081 lang [keyword, str] =
    case lang of
        _    -> "Scripts are case sensitive. Use '" ++ keyword ++ "', not '" ++ str ++ "' (or quote if literal)."

getLangMessage 1082 lang [] =
    case lang of
        _    -> "This file has a UTF-8 BOM. Remove it with: LC_CTYPE=C sed '1s/^...//' < yourscript ."

getLangMessage 1083 lang [c] =
    case lang of
        _    ->  "This " ++ c ++ " is literal. Check expression (missing ;/\\n?) or quote it."

getLangMessage 1084 lang [] =
    case lang of
        _    -> "Use #!, not !#, for the shebang."

getLangMessage 1086 lang [] =
    case lang of
        _    -> "Don't use $ on the iterator name in for loops."

getLangMessage 1087 lang [] =
    case lang of
        _    -> "Use braces when expanding arrays, e.g. ${array[idx]} (or ${var}[.. to quiet)."

getLangMessage 1088 lang [] =
    case lang of
        _    -> "Parsing stopped here. Invalid use of parentheses?"

getLangMessage 1089 lang [] =
    case lang of
        _    -> "Parsing stopped here. Is this keyword correctly matched up?"

getLangMessage 1090 lang [] =
    case lang of
        _    -> "ShellCheck can't follow non-constant source. Use a directive to specify location."

getLangMessage 1091 lang [err] =
    case lang of
        _    -> "Not following: " ++ err

getLangMessage 1092 lang [] =
    case lang of
        _    -> "Stopping at 100 'source' frames :O"

getLangMessage 1094 lang [] =
    case lang of
        _    -> "Parsing of sourced file failed. Ignoring it."

getLangMessage 1095 lang [] =
    case lang of
        _    -> "You need a space or linefeed between the function name and body."

getLangMessage 1097 lang [] =
    case lang of
        _    -> "Unexpected ==. For assignment, use =. For comparison, use [/[[. Or quote for literal string."

getLangMessage 1098 lang [] =
    case lang of
        _    -> "Quote/escape special characters when using eval, e.g. eval \"a=(b)\"."

getLangMessage 1099 lang [] =
    case lang of
        _    -> "You need a space before the # ."

getLangMessage 1100 lang [] =
    case lang of
        _    -> "This is a unicode dash. Delete and retype as ASCII minus."

getLangMessage 1101 lang [] =
    case lang of
        _    -> "Delete trailing spaces after \\ to break line (or use quotes for literal space)."

getLangMessage 1102 lang [] =
    case lang of
        _    -> "Shells disambiguate $(( differently or not at all. For $(command substitution), add space after $( . For $((arithmetics)), fix parsing errors."

getLangMessage 1103 lang [] =
    case lang of
        _    -> "This shell type is unknown. Use e.g. sh or bash."

getLangMessage 1104 lang [] =
    case lang of
        _    -> "Use #!, not just !, for the shebang."

getLangMessage 1105 lang [] =
    case lang of
        _    -> "Shells disambiguate (( differently or not at all. For subshell, add spaces around ( . For ((, fix parsing errors."

getLangMessage 1106 lang [alt, str] =
    case lang of
        _    -> "In arithmetic contexts, use " ++ alt ++ " instead of -" ++ str

getLangMessage 1107 lang [] =
    case lang of
        _    -> "This directive is unknown. It will be ignored."

getLangMessage 1108 lang [trailingOp] =
    case lang of
        _    -> "You need a space before and after the " ++ trailingOp ++ " ."

getLangMessage 1109 lang [] =
    case lang of
        _    -> "This is an unquoted HTML entity. Replace with corresponding character."

getLangMessage 1110 lang [] =
    case lang of
        _    -> "This is a unicode quote. Delete and retype it (or quote to make literal)."

getLangMessage 1111 lang [] =
    case lang of
        _    -> "This is a unicode quote. Delete and retype it (or ignore/singlequote for literal)."

getLangMessage 1112 lang [] =
    case lang of
        _    -> "This is a unicode quote. Delete and retype it (or ignore/doublequote for literal)."

getLangMessage 1113 lang [] =
    case lang of
        _    -> "Use #!, not just #, for the shebang."

getLangMessage 1114 lang [] =
    case lang of
        _    -> "Remove leading spaces before the shebang."

getLangMessage 1115 lang [] =
    case lang of
        _    -> "Remove spaces between # and ! in the shebang."

getLangMessage 1116 lang [] =
    case lang of
        _    -> "Missing $ on a $((..)) expression? (or use ( ( for arrays)."

getLangMessage 1117 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 1118 lang [] =
    case lang of
        _    -> "Delete whitespace after the here-doc end token."

getLangMessage 1119 lang [] =
    case lang of
        _    -> "Add a linefeed between end token and terminating ')'."

getLangMessage 1120 lang [] =
    case lang of
        _    -> "No comments allowed after here-doc token. Comment the next line instead."

getLangMessage 1121 lang [] =
    case lang of
        _    -> "Add ;/& terminators (and other syntax) on the line with the <<, not here."

getLangMessage 1122 lang [] =
    case lang of
        _    -> "Nothing allowed after end token. To continue a command, put it on the line with the <<."

getLangMessage 1123 lang [] =
    case lang of
        _    -> "ShellCheck directives are only valid in front of complete compound commands, like 'if', not e.g. individual 'elif' branches."

getLangMessage 1124 lang [] =
    case lang of
        _    -> "ShellCheck directives are only valid in front of complete commands like 'case' statements, not individual case branches."

getLangMessage 1125 lang [] =
    case lang of
        _    -> "Invalid key=value pair? Ignoring the rest of this directive starting here."

getLangMessage 1126 lang [] =
    case lang of
        _    -> "Place shellcheck directives before commands, not after."

getLangMessage 1127 lang [] =
    case lang of
        _    -> "Was this intended as a comment? Use # in sh."

getLangMessage 1128 lang [] =
    case lang of
        _    -> "The shebang must be on the first line. Delete blanks and move comments."

getLangMessage 1129 lang [] =
    case lang of
        _    -> "You need a space before the ! ."

getLangMessage 1130 lang [] =
    case lang of
        _    -> "You need a space before the : ."

getLangMessage 1131 lang [] =
    case lang of
        _    -> "Use 'elif' to start another branch."

getLangMessage 1132 lang [] =
    case lang of
        _    -> "This & terminates the command. Escape it or add space after & to silence."

getLangMessage 1133 lang [] =
    case lang of
        _    -> "Unexpected start of line. If breaking lines, |/||/&& should be at the end of the previous one."

getLangMessage 1134 lang [message] =
    case lang of
        _    -> message

getLangMessage 1135 lang [] =
    case lang of
        _    -> "Prefer escape over ending quote to make $ literal. Instead of \"It costs $\"5, use \"It costs \\$5\"."

getLangMessage 1136 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 1137 lang [c] =
    case lang of
        _    -> case c of 
            "(" -> "Missing second '(' to start arithmetic for ((;;)) loop"
            ")" -> "Missing second ')' to terminate 'for ((;;))' loop condition"
            _ -> "Unsupported message for 1137"

getLangMessage 1138 lang [c] =
    case lang of
        _    -> "Remove spaces between " ++ c ++ c ++ " in arithmetic for loop."

getLangMessage 1139 lang [alt, c] =
    case lang of
        _    ->  "Use " ++ alt ++ " instead of '" ++ c ++ "' between test commands."

getLangMessage 1140 lang [] =
    case lang of
        _    -> "Unexpected parameters after condition. Missing &&/||, or bad expression?"

getLangMessage 1141 lang [] =
    case lang of
        _    -> "Unexpected tokens after compound command. Bad redirection or missing ;/&&/||/|?"

getLangMessage 1142 lang [] =
    case lang of
        _    -> "Use 'done < <(cmd)' to redirect from process substitution (currently missing one '<')."

getLangMessage 1143 lang [] =
    case lang of
        _    -> "This backslash is part of a comment and does not continue the line."

getLangMessage 1144 lang [] =
    case lang of
        _    -> "external-sources can only be enabled in .shellcheckrc, not in individual files."

getLangMessage 1145 lang [] =
    case lang of
        _    -> "Unknown external-sources value. Expected true/false."

getLangMessage 2000 lang [] =
    case lang of
        _    -> "See if you can use ${#variable} instead."

getLangMessage 2001 lang [] =
    case lang of
        _    -> "See if you can use ${variable//search/replace} instead."

getLangMessage 2002 lang [] =
    case lang of
        _    -> "Useless cat. Consider 'cmd < file | ..' or 'cmd file | ..' instead."

getLangMessage 2003 lang [] =
    case lang of
        _    -> "expr is antiquated. Consider rewriting this using $((..)), ${} or [[ ]]."

getLangMessage 2004 lang [] =
    case lang of
        _    -> "$/${} is unnecessary on arithmetic variables."

getLangMessage 2005 lang [] =
    case lang of
        _    -> "Useless echo? Instead of 'echo $(cmd)', just use 'cmd'."

getLangMessage 2006 lang [] =
    case lang of
        _    -> "Use $(...) notation instead of legacy backticks `...`."

getLangMessage 2007 lang [] =
    case lang of
        _    -> "Use $((..)) instead of deprecated $[..]"

getLangMessage 2008 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2009 lang [] =
    case lang of
        _    -> "Consider using pgrep instead of grepping ps output."

getLangMessage 2010 lang [] =
    case lang of
        _    -> "Don't use ls | grep. Use a glob or a for loop with a condition to allow non-alphanumeric filenames."

getLangMessage 2011 lang [] =
    case lang of
        _    -> "Use 'find .. -print0 | xargs -0 ..' or 'find .. -exec .. +' to allow non-alphanumeric filenames."

getLangMessage 2012 lang [] =
    case lang of
        _    -> "Use find instead of ls to better handle non-alphanumeric filenames."

getLangMessage 2013 lang [] =
    case lang of
        _    -> "To read lines rather than words, pipe/redirect to a 'while read' loop."

getLangMessage 2014 lang [] =
    case lang of
        _    -> "This will expand once before find runs, not per file found."

getLangMessage 2015 lang [] =
    case lang of
        _    -> "Note that A && B || C is not if-then-else. C may run when A is true."

getLangMessage 2016 lang [] =
    case lang of
        _    -> "Expressions don't expand in single quotes, use double quotes for that."

getLangMessage 2017 lang [] =
    case lang of
        _    -> "Increase precision by replacing a/b*c with a*c/b."

getLangMessage 2018 lang [] =
    case lang of
        _    -> "Use '[:lower:]' to support accents and foreign alphabets."

getLangMessage 2019 lang [] =
    case lang of
        _    -> "Use '[:upper:]' to support accents and foreign alphabets."

getLangMessage 2020 lang [] =
    case lang of 
        _    -> "tr replaces sets of chars, not words (mentioned due to duplicates)."

getLangMessage 2021 lang [] =
    case lang of
        _    -> "Don't use [] around classes in tr, it replaces literal square brackets."

getLangMessage 2022 lang [char, wordStartingWith] =
    case lang of
        _    -> "Note that unlike globs, " ++ char ++ "* here matches '" ++ char ++ char ++ char ++ "' but not '" ++ wordStartingWith ++ "'."

getLangMessage 2023 lang [] =
    case lang of
        _    -> "The shell may override 'time' as seen in man time(1). Use 'command time ..' for that one."

getLangMessage 2024 lang [example] =
    case lang of
        _    -> "sudo doesn't affect redirects. Use " ++ example

getLangMessage 2025 lang [] =
    case lang of
        _    -> "Make sure all escape sequences are enclosed in \\[..\\] to prevent line wrapping issues"

getLangMessage 2026 lang [] =
    case lang of
        _    -> "This word is outside of quotes. Did you intend to 'nest '\"'single quotes'\"' instead'? "

getLangMessage 2027 lang [] =
    case lang of
        _    -> "The surrounding quotes actually unquote this. Remove or escape them."

getLangMessage 2028 lang [] =
    case lang of
        _    -> "echo may not expand escape sequences. Use printf."

getLangMessage 2029 lang [] =
    case lang of
        _    -> "Note that, unescaped, this expands on the client side."

getLangMessage 2030 lang [str, reason] =
    case lang of
        _    -> "Modification of " ++ str ++ " is local (to subshell caused by "++ reason ++")."

getLangMessage 2031 lang [str] =
    case lang of
        _    -> str ++ " was modified in a subshell. That change might be lost."

getLangMessage 2032 lang [cmd, patternContext] =
    case lang of
        _    -> "This function can't be invoked via " ++ cmd ++ patternContext

getLangMessage 2033 lang [] =
    case lang of
        _    -> "Shell functions can't be passed to external commands. Use separate script or sh -c."

getLangMessage 2034 lang [name] =
    case lang of
        _    -> name ++ " appears unused. Verify use (or export if used externally)."

getLangMessage 2035 lang [] =
    case lang of
        _    -> "Use ./*glob* or -- *glob* so names with dashes won't become options."

getLangMessage 2036 lang [] =
    case lang of
        _    -> "If you wanted to assign the output of the pipeline, use a=$(b | c) ."

getLangMessage 2037 lang [] =
    case lang of
        _    -> "To assign the output of a command, use var=$(cmd) ."

getLangMessage 2038 lang [] =
    case lang of
        _    -> "Use 'find .. -print0 | xargs -0 ..' or 'find .. -exec .. +' to allow non-alphanumeric filenames."

getLangMessage 2039 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2040 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2041 lang [] =
    case lang of
        _    -> "This is a literal string. To run as a command, use $(..) instead of '..' . "

getLangMessage 2042 lang [] =
    case lang of
        _    -> "Use spaces, not commas, to separate loop elements."

getLangMessage 2043 lang [] =
    case lang of
        _    -> "This loop will only ever run once. Bad quoting or missing glob/expansion?"

getLangMessage 2044 lang [] =
    case lang of
        _    -> "For loops over find output are fragile. Use find -exec or a while read loop."

getLangMessage 2045 lang [] =
    case lang of
        _    -> "Iterating over ls output is fragile. Use globs."

getLangMessage 2046 lang [] =
    case lang of
        _    -> "Quote this to prevent word splitting."

getLangMessage 2048 lang [suggestion] =
    case lang of
        _    -> "Use \""++ suggestion ++"\" (with quotes) to prevent whitespace problems."

getLangMessage 2049 lang [] =
    case lang of
        _    -> "=~ is for regex, but this looks like a glob. Use = instead."

getLangMessage 2050 lang [] =
    case lang of
        _    -> "This expression is constant. Did you forget the $ on a variable?"

getLangMessage 2051 lang [] =
    case lang of
        _    -> "Bash doesn't support variables in brace range expansions."

getLangMessage 2053 lang [op] =
    case lang of
        _    -> "Quote the right-hand side of " ++ op ++ " in [[ ]] to prevent glob matching."

getLangMessage 2054 lang [] =
    case lang of
        _    -> "Use spaces, not commas, to separate array elements."

getLangMessage 2055 lang [suggestion] =
    case lang of
        _    -> "You probably wanted " ++ suggestion ++ " here, otherwise it's always true."

getLangMessage 2056 lang [] =
    case lang of
        _    -> "You probably wanted && here, otherwise it's always true."

getLangMessage 2057 lang [] =
    case lang of
        _    -> "Unknown binary operator."

getLangMessage 2058 lang [] =
    case lang of
        _    -> "Unknown unary operator."

getLangMessage 2059 lang [] =
    case lang of
        _    -> "Don't use variables in the printf format string. Use printf '..%s..' \"$foo\"."

getLangMessage 2060 lang [] =
    case lang of
        _    -> "Quote parameters to tr to prevent glob expansion."

getLangMessage 2061 lang [s] =
    case lang of
        _    -> "Quote the parameter to " ++ s ++ " so the shell won't interpret it."

getLangMessage 2062 lang [] =
    case lang of
        _    -> "Quote the grep pattern so the shell won't interpret it."

getLangMessage 2063 lang [] =
    case lang of
        _    -> "Grep uses regex, but this looks like a glob."

getLangMessage 2064 lang [] =
    case lang of
        _    -> "Use single quotes, otherwise this expands now rather than when signalled."

getLangMessage 2065 lang [] =
    case lang of
        _    -> "This is interpreted as a shell file redirection, not a comparison."

getLangMessage 2066 lang [] =
    case lang of
        _    -> "Since you double quoted this, it will not word split, and the loop will only run once."

getLangMessage 2067 lang [] =
    case lang of
        _    -> "Missing ';' or + terminating -exec. You can't use |/||/&&, and ';' has to be a separate, quoted argument."

getLangMessage 2068 lang [] =
    case lang of
        _    -> "Double quote array expansions to avoid re-splitting elements."

getLangMessage 2069 lang [] =
    case lang of
        _    -> "To redirect stdout+stderr, 2>&1 must be last (or use '{ cmd > file; } 2>&1' to clarify)."

getLangMessage 2070 lang [] =
    case lang of
        _    -> "-n doesn't work with unquoted arguments. Quote or use [[ ]]."

getLangMessage 2071 lang [_, op, suggestion] =
    case lang of
        _    -> op ++ " is not a valid operator. " ++ "Use " ++ suggestion ++ " ."

getLangMessage 2071 lang [op, suggestion] =
    case lang of
        _    -> op ++ " is for string comparisons. Use " ++ suggestion ++ " instead."

getLangMessage 2072 lang [] =
    case lang of
        _    -> "Decimals are not supported. Either use integers only, or use bc or awk to compare."

getLangMessage 2073 lang [op] =
    case lang of
        _    -> "Escape \\" ++ op ++ " to prevent it redirecting"
        
getLangMessage 2073 lang [op, suggestion] =
    case lang of
        _    -> "Escape \\" ++ op ++ " to prevent it redirecting (or switch to "++ suggestion ++")." 

getLangMessage 2074 lang [] =
    case lang of
        _    -> "Can't use =~ in [ ]. Use [[..]] instead."

getLangMessage 2075 lang [op] =
    case lang of
        _    -> "Escaping " ++ op ++" is required in [..], but invalid in [[..]]"

getLangMessage 2076 lang [] =
    case lang of
        _    -> "Remove quotes from right-hand side of =~ to match as a regex rather than literally."
getLangMessage 2077 lang [] =
    case lang of
        _    -> "You need spaces around the comparison operator."

getLangMessage 2078 lang [] =
    case lang of
        _    -> "This expression is constant. Did you forget a $ somewhere?"

getLangMessage 2079 lang [] =
    case lang of
        _    -> "(( )) doesn't support decimals. Use bc or awk."

getLangMessage 2080 lang [] =
    case lang of
        _    -> "Numbers with leading 0 are considered octal."

getLangMessage 2081 lang ["Bash"] =
    case lang of
        _    -> "[ .. ] can't match globs. Use [[ .. ]] or case statement."

getLangMessage 2081 lang [] =
    case lang of
        _    -> "[ .. ] can't match globs. Use a case statement."

getLangMessage 2082 lang [] =
    case lang of
        _    -> "To expand via indirection, use arrays, ${!name} or (for sh only) eval."

getLangMessage 2083 lang [] =
    case lang of
        _    -> "Don't add spaces after the slash in './file'."

getLangMessage 2084 lang [] =
    case lang of
        _    -> "Remove '$' or use '_=$((expr))' to avoid executing output."

getLangMessage 2086 lang [] =
    case lang of
        _    -> "Double quote to prevent globbing and word splitting."

getLangMessage 2087 lang [message] =
    case lang of
        _    -> "Quote '" ++ message ++ "' to make here document expansions happen on the server side rather than on the client."

getLangMessage 2088 lang [] =
    case lang of
        _    -> "Tilde does not expand in quotes. Use $HOME."

getLangMessage 2089 lang [suggestion] =
    case lang of
        _    -> "Quotes/backslashes will be treated literally. " ++ suggestion

getLangMessage 2090 lang [] =
    case lang of
        _    -> "Quotes/backslashes in this variable will not be respected."

getLangMessage 2091 lang [] =
    case lang of
        _    -> "Remove surrounding $() to avoid executing output (or use eval if intentional)."

getLangMessage 2092 lang [] =
    case lang of
        _    -> "Remove backticks to avoid executing output (or use eval if intentional)."

getLangMessage 2093 lang [] =
    case lang of
        _    -> "Remove \"exec \" if script should continue after this command."

getLangMessage 2094 lang [] =
    case lang of
        _    -> "Make sure not to read and write the same file in the same pipeline."

getLangMessage 2095 lang [name] =
    case lang of
        _    -> name ++ " may swallow stdin, preventing this loop from working properly."
getLangMessage 2095 lang [name, flag] =
    case lang of
        _    -> "Use " ++ name ++ " " ++ flag ++ " to prevent " ++ name ++ " from swallowing stdin."

getLangMessage 2096 lang [] =
    case lang of
        _    -> "On most OS, shebangs can only specify a single parameter."

getLangMessage 2097 lang [] =
    case lang of
        _    -> "This assignment is only seen by the forked process."

getLangMessage 2098 lang [] =
    case lang of
        _    -> "This expansion will not see the mentioned assignment."

getLangMessage 2099 lang [op] =
    case lang of
        _    -> "Use $((..)) for arithmetics, e.g. i=$((i " ++ op ++ " 2))"

getLangMessage 2100 lang [op] =
    case lang of
        _    -> "Use $((..)) for arithmetics, e.g. i=$((i " ++ op ++ " 2))"

getLangMessage 2101 lang [] =
    case lang of
        _    -> "Named class needs outer [], e.g. [[:digit:]]."

getLangMessage 2102 lang [] =
    case lang of
        _    -> "Ranges can only match single chars (mentioned due to duplicates)."

getLangMessage 2103 lang [] =
    case lang of
        _    -> "Use a ( subshell ) to avoid having to cd back."

getLangMessage 2104 lang [name] =
    case lang of
        _    -> "In functions, use return instead of " ++ name ++ "."

getLangMessage 2105 lang [name] =
    case lang of
        _    -> name ++ " is only valid in loops."

getLangMessage 2106 lang [str] =
    case lang of
        _    -> "This only exits the subshell caused by the " ++ str ++ "."

getLangMessage 2107 lang [] =
    case lang of
        _    -> "Instead of [ a && b ], use [ a ] && [ b ]."

getLangMessage 2108 lang [] =
    case lang of
        _    -> "In [[..]], use && instead of -a."

getLangMessage 2109 lang [] =
    case lang of
        _    -> "Instead of [ a || b ], use [ a ] || [ b ]."

getLangMessage 2110 lang [] =
    case lang of
        _    -> "In [[..]], use || instead of -o."

getLangMessage 2111 lang [] =
    case lang of
        _    -> "ksh does not allow 'function' keyword and '()' at the same time."

getLangMessage 2112 lang [] =
    case lang of
        _    -> "'function' keyword is non-standard. Delete it."

getLangMessage 2113 lang [] =
    case lang of
        _    -> "'function' keyword is non-standard. Use 'foo()' instead of 'function foo'."

getLangMessage 2114 lang [] =
    case lang of
        _    -> "Warning: deletes a system directory."

getLangMessage 2115 lang [path] =
    case lang of
        _    -> "Use \"${var:?}\" to ensure this never expands to " ++ path ++ " ."

getLangMessage 2116 lang [] =
    case lang of
        _    -> "Useless echo? Instead of 'cmd $(echo foo)', just use 'cmd foo'."

getLangMessage 2117 lang [] =
    case lang of
        _    -> "To run commands as another user, use su -c or sudo."

getLangMessage 2118 lang [] =
    case lang of
        _    -> "Ksh does not support |&. Use 2>&1 |."

getLangMessage 2119 lang [suggestion] =
    case lang of
        _    -> "Use " ++ suggestion ++ " \"$@\" if function's $1 should mean script's $1."

getLangMessage 2120 lang [name] =
    case lang of
        _    -> name ++ " references arguments, but none are ever passed."

getLangMessage 2121 lang [] =
    case lang of
        _    -> "To assign a variable, use just 'var=value', no 'set ..'."

getLangMessage 2122 lang [op, esc, invert] =
    case lang of
        _    -> op ++ " is not a valid operator. " ++
                "Use '! a " ++ esc ++ invert ++ " b' instead."

getLangMessage 2123 lang [] =
    case lang of
        _    -> "PATH is the shell search path. Use another name."

getLangMessage 2124 lang [] =
    case lang of
        _    -> "Assigning an array to a string! Assign as array, or use * instead of @ to concatenate."

getLangMessage 2125 lang [] =
    case lang of
        _    -> "Brace expansions and globs are literal in assignments. Quote it or use an array."

getLangMessage 2126 lang [] =
    case lang of
        _    -> "Consider using 'grep -c' instead of 'grep|wc -l'."

getLangMessage 2127 lang [s, shell] =
    case lang of
        _    -> "To use " ++ s ++ ", specify #!/usr/bin/env " ++ shell

getLangMessage 2128 lang [] =
    case lang of
        _    -> "Expanding an array without an index only gives the first element."

getLangMessage 2129 lang [] =
    case lang of
        _    -> "Consider using { cmd1; cmd2; } >> file instead of individual redirects."

getLangMessage 2130 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2139 lang [] =
    case lang of
        _    -> "This expands when defined, not when used. Consider escaping."

getLangMessage 2140 lang [value] =
    case lang of
        _    -> "Word is of the form \"A\"B\"C\" (B indicated). Did you mean \"ABC\" or \"A\\\"B\\\"C\"?"

getLangMessage 2141 lang [suggestion] =
    case lang of
        _    -> "This backslash is literal. Did you mean IFS=" ++ suggestion ++ " ?"

getLangMessage 2141 lang ["", desc] =
    case lang of
        _    -> "This IFS value contains " ++ desc ++ ". For tabs/linefeeds/escapes, use $'..', literal, or printf."

getLangMessage 2142 lang [] =
    case lang of
        _    -> "Aliases can't use positional parameters. Use a function."

getLangMessage 2143 lang [flip, name, op] =
    case lang of
        _    -> "Use " ++ flip ++ name ++ " -q instead of " ++
                "comparing output with [ " ++ op ++ " .. ]."

getLangMessage 2144 lang [op] =
    case lang of
        _    -> op ++ " doesn't work with globs. Use a for loop."

getLangMessage 2145 lang [] =
    case lang of
        _    -> "Argument mixes string and array. Use * or separate argument."

getLangMessage 2146 lang [] =
    case lang of
        _    -> "This action ignores everything before the -o. Use \\( \\) to group."

getLangMessage 2147 lang [] =
    case lang of
        _    -> "Literal tilde in PATH works poorly across programs."

getLangMessage 2148 lang [] =
    case lang of
        _    -> "Tips depend on target shell and yours is unknown. Add a shebang or a 'shell' directive."

getLangMessage 2149 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2150 lang [] =
    case lang of
        _    -> "-exec does not invoke a shell. Rewrite or use -exec sh -c .. ."

getLangMessage 2151 lang [] =
    case lang of
        _    -> "Only one integer 0-255 can be returned. Use stdout for other data."

getLangMessage 2152 lang [] =
    case lang of
        _    -> "Can only return 0-255. Other data should be written to stdout."

getLangMessage 2153 lang [var, match] =
    case lang of
        _    -> "Possible misspelling: " ++ var ++ " may not be assigned. Did you mean " ++ match ++ "?"

getLangMessage 2154 lang [var, optionalTip] =
    case lang of
        _    -> var ++ " is referenced but not assigned" ++ optionalTip ++ "."

getLangMessage 2155 lang [] =
    case lang of
        _    -> "Declare and assign separately to avoid masking return values."

getLangMessage 2156 lang [] =
    case lang of
        _    -> "Injecting filenames is fragile and insecure. Use parameters."

getLangMessage 2157 lang [op, result] =
    case lang of
        _    -> "Argument to " ++ op ++ " is always " ++ result ++ " due to literal strings."

getLangMessage 2158 lang [] =
    case lang of
        _    -> "[ false ] is true. Remove the brackets."

getLangMessage 2159 lang [] =
    case lang of
        _    -> "[ 0 ] is true. Use 'false' instead."

getLangMessage 2160 lang [] =
    case lang of
        _    -> "Instead of '[ true ]', just use 'true'."

getLangMessage 2161 lang [] =
    case lang of
        _    -> "Instead of '[ 1 ]', use 'true'."

getLangMessage 2162 lang [] =
    case lang of
        _    -> "read without -r will mangle backslashes."

getLangMessage 2163 lang [name] =
    case lang of
        _    -> "This does not export '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

getLangMessage 2164 lang [name] =
    case lang of
        _    -> "Use '" ++ name ++ " ... || exit' or '" ++ name ++ " ... || return' in case " ++ name ++ " fails."

getLangMessage 2165 lang [] =
    case lang of
        _    -> "This nested loop overrides the index variable of its parent."

getLangMessage 2166 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2167 lang [] =
    case lang of
        _    -> "This parent loop has its index variable overridden."

getLangMessage 2168 lang [] =
    case lang of
        _    -> "'local' is only valid in functions."

getLangMessage 2169 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2170 lang [op, suggestion, fix, kind] =
    case lang of
        _    -> "Invalid number for " ++ op ++ ". Use " ++ suggestion ++
                        " to compare as string (or use " ++ fix ++
                        " to expand as " ++ kind ++ ")."

getLangMessage 2171 lang [str, opposite] =
    case lang of
        _    -> "Found trailing " ++ str ++ " outside test. Add missing " ++ opposite ++ " or quote if intentional."

getLangMessage 2172 lang [] =
    case lang of
        _    -> "Trapping signals by number is not well defined. Prefer signal names."

getLangMessage 2173 lang [] =
    case lang of
        _    -> "SIGKILL/SIGSTOP can not be trapped."

getLangMessage 2174 lang [] =
    case lang of
        _    -> "When used with -p, -m only applies to the deepest directory."

getLangMessage 2175 lang [] =
    case lang of
        _    -> "Quote this invalid brace expansion since it should be passed literally to eval."

getLangMessage 2176 lang [] =
    case lang of
        _    -> "'time' is undefined for pipelines. time single stage or bash -c instead."

getLangMessage 2177 lang [] =
    case lang of
        _    -> "'time' is undefined for compound commands, time sh -c instead."

getLangMessage 2178 lang [] =
    case lang of
        _    -> "Variable was used as an array but is now assigned a string."

getLangMessage 2179 lang [] =
    case lang of
        _    -> "Use array+=(\"item\") to append items to an array."

getLangMessage 2180 lang [] =
    case lang of
        _    -> "Bash does not support multidimensional arrays. Use 1D or associative arrays."

getLangMessage 2181 lang [symbol] =
    case lang of
        _    -> "Check exit code directly with e.g. 'if " ++ symbol ++ "mycmd;', not indirectly with $?."

getLangMessage 2182 lang [] =
    case lang of
        _    -> "This printf format string has no variables. Other arguments are ignored."

getLangMessage 2183 lang [formatCount, argCount] =
    case lang of
        _    -> "This format string has " ++ formatCount ++ " " ++ if formatCount == "1" || formatCount == "0" then "variable" else "variables" ++
                        ", but is passed " ++ argCount ++ if argCount == "1" || argCount == "0" then " argument" else " arguments" ++ "."

getLangMessage 2184 lang [] =
    case lang of
        _    -> "Quote arguments to unset so they're not glob expanded."

getLangMessage 2185 lang [] =
    case lang of
        _    -> "Some finds don't have a default path. Specify '.' explicitly."

getLangMessage 2186 lang [] =
    case lang of
        _    -> "tempfile is deprecated. Use mktemp instead."

getLangMessage 2187 lang [] =
    case lang of
        _    -> "Ash scripts will be checked as Dash. Add '# shellcheck shell=dash' to silence."

getLangMessage 2188 lang [] =
    case lang of
        _    -> "This redirection doesn't have a command. Move to its command (or use 'true' as no-op)."

getLangMessage 2189 lang [] =
    case lang of
        _    -> "You can't have | between this redirection and the command it should apply to."

getLangMessage 2190 lang [] =
    case lang of
        _    -> "Elements in associative arrays need index, e.g. array=( [index]=value ) ."

getLangMessage 2191 lang [] =
    case lang of
        _    -> "The = here is literal. To assign by index, use ( [index]=value ) with no spaces. To keep as literal, quote it."

getLangMessage 2192 lang [] =
    case lang of
        _    -> "This array element has no value. Remove spaces after = or use \"\" for empty string."

getLangMessage 2193 lang [] =
    case lang of
        _    -> "The arguments to this comparison can never be equal. Make sure your syntax is correct."

getLangMessage 2194 lang [] =
    case lang of
        _    -> "This word is constant. Did you forget the $ on a variable?"

getLangMessage 2195 lang [] =
    case lang of
        _    -> "This pattern will never match the case statement's word. Double check them."

getLangMessage 2196 lang [] =
    case lang of
        _    -> "egrep is non-standard and deprecated. Use grep -E instead."

getLangMessage 2197 lang [] =
    case lang of
        _    -> "fgrep is non-standard and deprecated. Use grep -F instead."

getLangMessage 2198 lang [] =
    case lang of
        _    -> "Arrays don't work as operands in [ ]. Use a loop (or concatenate with * instead of @)."

getLangMessage 2199 lang [] =
    case lang of
        _    -> "Arrays implicitly concatenate in [[ ]]. Use a loop (or explicit * instead of @)."

getLangMessage 2200 lang [] =
    case lang of
        _    -> "Brace expansions don't work as operands in [ ]. Use a loop."

getLangMessage 2201 lang [] =
    case lang of
        _    -> "Brace expansion doesn't happen in [[ ]]. Use a loop."

getLangMessage 2202 lang [] =
    case lang of
        _    -> "Globs don't work as operands in [ ]. Use a loop."

getLangMessage 2203 lang [] =
    case lang of
        _    -> "Globs are ignored in [[ ]] except right of =/!=. Use a loop."

getLangMessage 2204 lang [] =
    case lang of
        _    -> "(..) is a subshell. Did you mean [ .. ], a test expression?"

getLangMessage 2205 lang [] =
    case lang of
        _    -> "(..) is a subshell. Did you mean [ .. ], a test expression?"

getLangMessage 2206 lang [suggestion1, suggestion2] =
    case lang of
        _    -> "Quote to prevent word splitting/globbing, or split robustly with " ++ suggestion1 ++ " or " ++ suggestion2 ++ "."

getLangMessage 2207 lang [suggestion1, suggestion2] =
    case lang of
        _    -> "Prefer " ++ suggestion1 ++ " or " ++ suggestion2 ++ " to split command output (or quote to avoid splitting)."

getLangMessage 2208 lang [] =
    case lang of
        _    -> "Use [[ ]] or quote arguments to -v to avoid glob expansion."

getLangMessage 2209 lang [] =
    case lang of
        _    -> "Use var=$(command) to assign output (or quote to assign string)."

getLangMessage 2210 lang [] =
    case lang of
        _    -> "This is a file redirection. Was it supposed to be a comparison or fd operation?"

getLangMessage 2211 lang [] =
    case lang of
        _    -> "This is a glob used as a command name. Was it supposed to be in ${..}, array, or is it missing quoting?"

getLangMessage 2212 lang [] =
    case lang of
        _    -> "Use 'false' instead of empty [/[[ conditionals."

getLangMessage 2213 lang [message] =
    case lang of
        _    -> "getopts specified -" ++ message ++ ", but it's not handled by this 'case'."

getLangMessage 2214 lang [] =
    case lang of
        _    -> "This case is not specified by getopts."

getLangMessage 2215 lang [] =
    case lang of
        _    -> "This flag is used as a command name. Bad line break or missing [ .. ]?"

getLangMessage 2216 lang [name, suggestion] =
    case lang of
        _    -> "Piping to '" ++ name ++ "', a command that doesn't read stdin. " ++ suggestion

getLangMessage 2217 lang [name, suggestion] =
    case lang of
        _    ->  "Redirecting to '" ++ name ++ "', a command that doesn't read stdin. " ++ suggestion

getLangMessage 2218 lang [] =
    case lang of
        _    -> "This function is only defined later. Move the definition up."

getLangMessage 2219 lang [] =
    case lang of
        _    -> "Instead of 'let expr', prefer (( expr )) ."

getLangMessage 2220 lang [] =
    case lang of
        _    -> "Invalid flags are not handled. Add a *) case."

getLangMessage 2221 lang [line] =
    case lang of
        _    -> "This pattern always overrides a later one on line" ++ line ++ "."

getLangMessage 2221 lang [] =
    case lang of
        _    -> "This pattern always overrides a later one."

getLangMessage 2222 lang [line] =
    case lang of
        _    -> "This pattern never matches because of a previous pattern on line" ++ line ++ "."

getLangMessage 2222 lang [] =
    case lang of
        _    -> "This pattern never matches because of a previous pattern."

getLangMessage 2223 lang [] =
    case lang of
        _    -> "This default assignment may cause DoS due to globbing. Quote it."

getLangMessage 2224 lang [] =
    case lang of
        _    -> "This mv has no destination. Check the arguments."

getLangMessage 2225 lang [] =
    case lang of
        _    -> "This cp has no destination. Check the arguments."

getLangMessage 2226 lang [] =
    case lang of
        _    -> "This ln has no destination. Check the arguments, or specify '.' explicitly."

getLangMessage 2227 lang [] =
    case lang of
        _    -> "Redirection applies to the find command itself. Rewrite to work per action (or move to end)."

getLangMessage 2229 lang [name] =
    case lang of
        _    -> "This does not read '" ++ name ++ "'. Remove $/${} for that, or use ${var?} to quiet."

getLangMessage 2230 lang [] =
    case lang of
        _    -> "'which' is non-standard. Use builtin 'command -v' instead."

getLangMessage 2231 lang [] =
    case lang of
        _    -> "Quote expansions in this for loop glob to prevent wordsplitting, e.g. \"$dir\"/*.txt ."

getLangMessage 2232 lang [command] =
    case lang of
        _    -> "Can't use sudo with builtins like " ++ command ++ ". Did you want sudo sh -c .. instead?"

getLangMessage 2233 lang [] =
    case lang of
        _    -> "Remove superfluous (..) around condition to avoid subshell overhead."

getLangMessage 2234 lang [] =
    case lang of
        _    -> "Remove superfluous (..) around test command to avoid subshell overhead."

getLangMessage 2235 lang [] =
    case lang of
        _    -> "Use { ..; } instead of (..) to avoid subshell overhead."

getLangMessage 2236 lang [now, suggestion] =
    case lang of
        _    -> "Use " ++ suggestion ++ " instead of " ++ now ++ "."

getLangMessage 2237 lang [now, suggestion] =
    case lang of
        _    -> "Use " ++ suggestion ++ " instead of " ++ now ++ "."

getLangMessage 2238 lang [] =
    case lang of
        _    -> "Redirecting to/from command name instead of file. Did you want pipes/xargs (or quote to ignore)?"

getLangMessage 2239 lang [] =
    case lang of
        _    -> "Ensure the shebang uses an absolute path to the interpreter."

getLangMessage 2240 lang [] =
    case lang of
        _    -> "The dot command does not support arguments in sh/dash. Set them as variables."

getLangMessage 2241 lang [] =
    case lang of
        _    -> "The exit status can only be one integer 0-255. Use stdout for other data."

getLangMessage 2242 lang [] =
    case lang of
        _    -> "Can only exit with status 0-255. Other data should be written to stdout/stderr."

getLangMessage 2243 lang [] =
    case lang of
        _    -> "Prefer explicit -n to check for output (or run command without [/[[ to check for success)."

getLangMessage 2244 lang [] =
    case lang of
        _    -> "Prefer explicit -n to check non-empty string (or use =/-ne to check boolean/integer)."

getLangMessage 2245 lang [op] =
    case lang of
        _    -> op ++ " only applies to the first expansion of this glob. Use a loop to check any/all."

getLangMessage 2246 lang [] =
    case lang of
        _    -> "This shebang specifies a directory. Ensure the interpreter is a file."

getLangMessage 2247 lang [] =
    case lang of
        _    -> "Flip leading $ and \" if this should be a quoted substitution."

getLangMessage 2248 lang [] =
    case lang of
        _    -> "Prefer double quoting even when variables don't contain special characters."

getLangMessage 2249 lang [] =
    case lang of
        _    -> "Consider adding a default *) case, even if it just exits with error."

getLangMessage 2250 lang [] =
    case lang of
        _    -> "Prefer putting braces around variable references even when not strictly required."

getLangMessage 2251 lang [] =
    case lang of
        _    -> "This ! is not on a condition and skips errexit. Use `&& exit 1` instead, or make sure $? is checked."

getLangMessage 2252 lang [] =
    case lang of
        _    -> "You probably wanted && here, otherwise it's always true."

getLangMessage 2253 lang [] =
    case lang of
        _    -> "Use -R to recurse, or explicitly a-r to remove read permissions."

getLangMessage 2254 lang [] =
    case lang of
        _    -> "Quote expansions in case patterns to match literally rather than as a glob."

getLangMessage 2255 lang [] =
    case lang of
        _    -> "[ ] does not apply arithmetic evaluation. Evaluate with $((..)) for numbers, or use string comparator for strings."

getLangMessage 2256 lang [] =
    case lang of
        _    -> "This translated string is the name of a variable. Flip leading $ and \" if this should be a quoted substitution."

getLangMessage 2257 lang [] =
    case lang of
        _    -> "Arithmetic modifications in command redirections may be discarded. Do them separately."

getLangMessage 2258 lang [] =
    case lang of
        _    -> "The trailing comma is part of the value, not a separator. Delete or quote it."

getLangMessage 2259 lang [] =
    case lang of
        _    -> "This redirection overrides piped input. To use both, merge or pass filenames."

getLangMessage 2260 lang [] =
    case lang of
        _    -> "This redirection overrides the output pipe. Use 'tee' to output to both."

getLangMessage 2261 lang [fd] =
    case lang of
        _    -> "Multiple redirections compete for " ++ fd ++ ". Use cat, tee, or pass filenames instead."

getLangMessage 2262 lang [] =
    case lang of
        _    -> "This alias can't be defined and used in the same parsing unit. Use a function instead."

getLangMessage 2263 lang [] =
    case lang of
        _    -> "Since they're in the same parsing unit, this command will not refer to the previously mentioned alias."

getLangMessage 2264 lang [] =
    case lang of
        _    -> "This function unconditionally re-invokes itself. Missing 'command'?"

getLangMessage 2265 lang [] =
    case lang of
        _    -> "Use && for logical AND. Single & will background and return true."

getLangMessage 2266 lang [] =
    case lang of
        _    -> "Use || for logical OR. Single | will pipe."

getLangMessage 2267 lang [] =
    case lang of
        _    -> "GNU xargs -i is deprecated in favor of -I{}"

getLangMessage 2268 lang [] =
    case lang of
        _    -> "Avoid x-prefix in comparisons as it no longer serves a purpose."

getLangMessage 2269 lang [] =
    case lang of
        _    -> "This variable is assigned to itself, so the assignment does nothing."

getLangMessage 2270 lang [] =
    case lang of
        _    -> "To assign positional parameters, use 'set -- first second ..' (or use [ ] to compare)."

getLangMessage 2271 lang [] =
    case lang of
        _    -> "For indirection, use arrays, declare \"var$n=value\", or (for sh) read/eval."

getLangMessage 2272 lang [] =
    case lang of
        _    -> "Command name contains ==. For comparison, use [ \"$var\" = value ]."

getLangMessage 2273 lang [] =
    case lang of
        _    -> "Sequence of ===s found. Merge conflict or intended as a commented border?"

getLangMessage 2274 lang [] =
    case lang of
        _    -> "Command name starts with ===. Intended as a commented border?"

getLangMessage 2275 lang [] =
    case lang of
        _    -> "Command name starts with =. Bad line break?"

getLangMessage 2276 lang [] =
    case lang of
        _    -> "This is interpreted as a command name containing '='. Bad assignment or comparison?"

getLangMessage 2277 lang [] =
    case lang of
        _    -> "Use BASH_ARGV0 to assign to $0 in bash (or use [ ] to compare)."

getLangMessage 2278 lang [] =
    case lang of
        _    -> "$0 can't be assigned in Ksh (but it does reflect the current function)."

getLangMessage 2279 lang [shell] =
    case lang of
        _    -> "$0 can't be assigned in " ++ shell ++ ". This becomes a command name."

getLangMessage 2280 lang [] =
    case lang of
        _    -> "$0 can't be assigned this way, and there is no portable alternative."

getLangMessage 2281 lang [current] =
    case lang of
        _    -> "Don't use " ++ current ++ " on the left side of assignments."

getLangMessage 2282 lang [] =
    case lang of
        _    -> "Variable names can't start with numbers, so this is interpreted as a command."

getLangMessage 2283 lang [] =
    case lang of
        _    -> "Remove spaces around = to assign (or use [ ] to compare, or quote '=' if literal)."

getLangMessage 2284 lang [] =
    case lang of
        _    -> "Use [ x = y ] to compare values (or quote '==' if literal)."

getLangMessage 2285 lang [] =
    case lang of
        _    -> "Remove spaces around += to assign (or quote '+=' if literal)."

getLangMessage 2286 lang [] =
    case lang of
        _    -> "This empty string is interpreted as a command name. Double check syntax (or use 'true' as a no-op)."

getLangMessage 2287 lang [] =
    case lang of
        _    -> "This is interpreted as a command name ending with '/'. Double check syntax."

getLangMessage 2288 lang [last] =
    case lang of
        _    -> "This is interpreted as a command name ending with " ++ last ++ ". Double check syntax."

getLangMessage 2289 lang [symbol] =
    case lang of
        _    -> "This is interpreted as a command name containing a " ++ symbol ++ ". Double check syntax."

getLangMessage 2290 lang ["="] =
    case lang of
        _    -> "Remove spaces around = to assign."

getLangMessage 2290 lang ["+="] =
    case lang of
        _    -> "Remove spaces around += to append."

getLangMessage 2291 lang [] =
    case lang of
        _    -> "Quote repeated spaces to avoid them collapsing into one."

getLangMessage 2292 lang [] =
    case lang of
        _    -> "Prefer [[ ]] over [ ] for tests in Bash/Ksh/Busybox."

getLangMessage 2293 lang [] =
    case lang of
        _    -> "When eval'ing @Q-quoted words, use * rather than @ as the index."

getLangMessage 2294 lang [] =
    case lang of
        _    -> "eval negates the benefit of arrays. Drop eval to preserve whitespace/symbols (or eval as string)."

getLangMessage 2295 lang [] =
    case lang of
        _    -> "Expansions inside ${..} need to be quoted separately, otherwise they match as patterns."

getLangMessage 2296 lang [symbol] =
    case lang of
        _    -> "Parameter expansions can't start with " ++ symbol ++ ". Double check syntax."

getLangMessage 2297 lang [] =
    case lang of
        _    -> "Double quotes must be outside ${}: ${\"invalid\"} vs \"${valid}\"."

getLangMessage 2298 lang [symbol] =
    case lang of
        _    -> symbol ++ " is invalid. For expansion, use ${x}. For indirection, use arrays, ${!x} or (for sh) eval."

getLangMessage 2299 lang [] =
    case lang of
        _    -> "Parameter expansions can't be nested. Use temporary variables."

getLangMessage 2300 lang [] =
    case lang of
        _    -> "Parameter expansion can't be applied to command substitutions. Use temporary variables."

getLangMessage 2301 lang [name] =
    case lang of
        _    -> "Parameter expansion starts with unexpected " ++ name ++ ". Double check syntax."

getLangMessage 2302 lang [] =
    case lang of
        _    -> "This loops over values. To loop over keys, use \"${!array[@]}\"."

getLangMessage 2303 lang [name] =
    case lang of
        _    -> name ++ " is an array value, not a key. Use directly or loop over keys instead."

getLangMessage 2304 lang [] =
    case lang of
        _    -> "* must be escaped to multiply: \\*. Modern $((x * y)) avoids this issue."

getLangMessage 2305 lang [] =
    case lang of
        _    -> "Quote regex argument to expr to avoid it expanding as a glob."

getLangMessage 2306 lang [] =
    case lang of
        _    -> "Escape glob characters in arguments to expr to avoid pathname expansion."

getLangMessage 2307 lang [number] =
    case lang of
        _    -> "'expr' expects 3+ arguments but sees " ++ number ++ ". Make sure each operator/operand is a separate argument, and escape <>&|."

getLangMessage 2308 lang [current, suggestion] =
    case lang of
        _    -> "'" ++ current ++ "' has unspecified results. Prefer '" ++ suggestion ++ "'."

getLangMessage 2309 lang [op, kind, suggestion, fix] =
    case lang of
        _    -> op ++ " treats this as " ++ kind ++ ". " ++
                            "Use " ++ suggestion ++ " to compare as string (or expand explicitly with " ++ fix ++ ")."

getLangMessage 2310 lang [condType] =
    case lang of
        _    -> "This function is invoked in " ++ condType ++ " so set -e will be disabled. Invoke separately if failures should cause the script to exit."

getLangMessage 2311 lang [] =
    case lang of
        _    -> "Bash implicitly disabled set -e for this function invocation because it's inside a command substitution. Add set -e; before it or enable inherit_errexit."
getLangMessage 2312 lang [] =
    case lang of
        _    -> "Consider invoking this command separately to avoid masking its return value (or use '|| true' to ignore)."

getLangMessage 2313 lang [] =
    case lang of
        _    -> "Quote array indices to avoid them expanding as globs."

getLangMessage 2314 lang [lastCommand] =
    case lang of
        _    -> "In Bats, ! will not fail the test if it is not the last command anymore. Use `run ! ` (on Bats >= 1.5.0) instead."

getLangMessage 2314 lang [] =
    case lang of
        _    -> "In Bats, ! does not cause a test failure. Use 'run ! ' (on Bats >= 1.5.0) instead."

getLangMessage 2315 lang [lastCommand] =
    case lang of
        _    -> "In Bats, ! will not fail the test if it is not the last command anymore. Fold the `!` into the conditional!"

getLangMessage 2315 lang [] =
    case lang of
        _    -> "In Bats, ! does not cause a test failure. Fold the `!` into the conditional!"

getLangMessage 2316 lang [cmd, lit] =
    case lang of
        _    -> "This applies " ++ cmd ++ " to the variable named " ++ lit ++
                 ", which is probably not what you want. Use a separate command or the appropriate `declare` options instead."

getLangMessage 2317 lang [] =
    case lang of
        _    -> "Command appears to be unreachable. Check usage (or ignore if invoked indirectly)."

getLangMessage 2318 lang [cmd] =
    case lang of
        _    -> "This assignment is used again in this '" ++ cmd ++ "', but won't have taken effect. Use two '" ++ cmd ++ "'s."

getLangMessage 2319 lang [] =
    case lang of
        _    -> "This $? refers to a condition, not a command. Assign to a variable to avoid it being overwritten."

getLangMessage 2320 lang [] =
    case lang of
        _    -> "This $? refers to echo/printf, not a previous command. Assign to variable to avoid it being overwritten."

getLangMessage 2321 lang [] =
    case lang of
        _    -> "Array indices are already arithmetic contexts. Prefer removing the $(( and ))."

getLangMessage 2322 lang [] =
    case lang of
        _    -> "In arithmetic contexts, ((x)) is the same as (x). Prefer only one layer of parentheses."

getLangMessage 2323 lang [str] =
    case lang of
        _    -> str ++ ". Prefer not wrapping in additional parentheses."

getLangMessage 2324 lang [] =
    case lang of
        _    -> "var+=1 will append, not increment. Use (( var += 1 )), typeset -i var, or quote number to silence."

getLangMessage 2325 lang [] =
    case lang of
        _    -> "Multiple ! in front of pipelines are a bash/ksh extension. Use only 0 or 1."

getLangMessage 2326 lang [] =
    case lang of
        _    -> "! is not allowed in the middle of pipelines. Use command group as in cmd | { ! cmd; } if necessary."

getLangMessage 2327 lang [] =
    case lang of
        _    -> "This command substitution will be empty because the command's output gets redirected away."

getLangMessage 2328 lang [suggestion] =
    case lang of
        _    -> "This redirection takes output away from the command substitution (use " ++ suggestion ++" to duplicate)."

getLangMessage 2328 lang [] =
    case lang of
        _    -> "This redirection takes output away from the command substitution"

getLangMessage 2329 lang [] =
    case lang of
        _    -> "Script contains undefined variable"

getLangMessage 2330 lang [] =
    case lang of
        _    -> "BusyBox [[ .. ]] does not support glob matching. Use a case statement."

getLangMessage 3001 lang [] =
    case lang of
        _    -> "process substitution is"

getLangMessage 3002 lang [] =
    case lang of
        _    -> "extglob is"

getLangMessage 3003 lang [] =
    case lang of
        _    -> "$'..' is"

getLangMessage 3004 lang [] =
    case lang of
        _    -> "$\"..\" is"

getLangMessage 3005 lang [] =
    case lang of
        _    -> "arithmetic for loops are"

getLangMessage 3006 lang [] =
    case lang of
        _    -> "standalone ((..)) is"

getLangMessage 3007 lang [] =
    case lang of
        _    -> "$[..] in place of $((..)) is"

getLangMessage 3008 lang [] =
    case lang of
        _    -> "select loops are"

getLangMessage 3009 lang [] =
    case lang of
        _    -> "brace expansion is"

getLangMessage 3010 lang [] =
    case lang of
        _    -> "[[ ]] is"

getLangMessage 3011 lang [] =
    case lang of
        _    -> "here-strings are"

getLangMessage 3012 lang [op] =
    case lang of
        _    -> "lexicographical " ++ op ++ " is"

getLangMessage 3013 lang [op] =
    case lang of
        _    -> op ++ " is"

getLangMessage 3014 lang [] =
    case lang of
        _    -> "== in place of = is"

getLangMessage 3015 lang [] =
    case lang of
        _    -> "=~ regex matching is"

getLangMessage 3016 lang [] =
    case lang of
        _    -> "unary -v (in place of [ -n \"${var+x}\" ]) is"

getLangMessage 3017 lang [] =
    case lang of
        _    -> "unary -a in place of -e is"

getLangMessage 3018 lang [op] =
    case lang of
        _    -> op ++ " is"

getLangMessage 3019 lang [] =
    case lang of
        _    -> "exponentials are"

getLangMessage 3020 lang [] =
    case lang of
        _    -> "&> is"

getLangMessage 3021 lang [] =
    case lang of
        _    -> ">& filename (as opposed to >& fd) is"

getLangMessage 3022 lang [] =
    case lang of
        _    -> "named file descriptors are"

getLangMessage 3023 lang [] =
    case lang of
        _    -> "FDs outside 0-9 are"

getLangMessage 3024 lang [] =
    case lang of
        _    -> "+= is"

getLangMessage 3025 lang [] =
    case lang of
        _    -> "/dev/{tcp,udp} is"

getLangMessage 3026 lang [] =
    case lang of
        _    -> "^ in place of ! in glob bracket expressions is"

getLangMessage 3028 lang [str] =
    case lang of
        _    -> str ++ " is"

getLangMessage 3029 lang [] =
    case lang of
        _    -> "|& in place of 2>&1 | is"

getLangMessage 3030 lang [] =
    case lang of
        _    -> "arrays are"

getLangMessage 3031 lang [] =
    case lang of
        _    -> "redirecting to/from globs is"

getLangMessage 3032 lang [] =
    case lang of
        _    -> "coproc is"

getLangMessage 3033 lang [] =
    case lang of
        _    -> "naming functions outside [a-zA-Z_][a-zA-Z0-9_]* is"

getLangMessage 3034 lang [] =
    case lang of
        _    -> "$(<file) to read files is"

getLangMessage 3035 lang [] =
    case lang of
        _    -> "`<file` to read files is"

getLangMessage 3036 lang flags =
    case lang of
        _    -> "echo flags besides" ++ foldl (\acc x -> if acc == "" then x else acc ++ " and " ++ x) "" flags

getLangMessage 3037 lang [] =
    case lang of
        _    -> "echo flags are"

getLangMessage 3038 lang [] =
    case lang of
        _    -> "exec flags are"

getLangMessage 3039 lang [] =
    case lang of
        _    -> "'let' is"

getLangMessage 3040 lang [opt] =
    case lang of
        _    -> "set option " ++ opt ++ " is"

getLangMessage 3041 lang [flag] =
    case lang of
        _    -> "set flag " ++ flag

getLangMessage 3042 lang [flag] =
    case lang of
        _    -> "set flag " ++ flag ++ " is"

getLangMessage 3043 lang [] =
    case lang of
        _    -> "'local' is"

getLangMessage 3044 lang [name] =
    case lang of
        _    -> "'" ++ name ++ "' is"

getLangMessage 3045 lang [name, flag] =
    case lang of
        _    -> name ++ " -" ++ flag ++ " is"

getLangMessage 3046 lang [] =
    case lang of
        _    -> "'source' in place of '.' is"

getLangMessage 3047 lang [str] =
    case lang of
        _    -> "trapping " ++ str ++ " is"

getLangMessage 3048 lang [] =
    case lang of
        _    -> "prefixing signal names with 'SIG' is"

getLangMessage 3049 lang [] =
    case lang of
        _    -> "using lower/mixed case for signal names is"

getLangMessage 3050 lang [] =
    case lang of
        _    -> "printf %q is"

getLangMessage 3051 lang [] =
    case lang of
        _    -> "'source' in place of '.' is"

getLangMessage 3052 lang [] =
    case lang of
        _    -> "arithmetic base conversion is"

getLangMessage 3053 lang [] =
    case lang of
        _    -> "indirect expansion is"

getLangMessage 3054 lang [] =
    case lang of
        _    -> "array references are"

getLangMessage 3055 lang [] =
    case lang of
        _    -> "array key expansion is"

getLangMessage 3056 lang [] =
    case lang of
        _    -> "name matching prefixes are"

getLangMessage 3057 lang [] =
    case lang of
        _    -> "string indexing is"

getLangMessage 3058 lang [] =
    case lang of
        _    -> "string operations on $@/$* are"

getLangMessage 3059 lang [] =
    case lang of
        _    -> "case modification is"

getLangMessage 3060 lang [] =
    case lang of
        _    -> "string replacement is"

getLangMessage 3061 lang [] =
    case lang of
        _    -> "read without a variable is"

getLangMessage _ _ _ = "不支持的方法"

warpShellSupportLangMessage Dash lang s = 
    case lang of
        _ -> "In dash, " ++ s ++ " not supported."

warpShellSupportLangMessage _  lang s =
    case lang of
        _ -> "In POSIX sh, " ++ s ++ " undefined."

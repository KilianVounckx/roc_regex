interface Parser
    exposes [
        ParseError,
        parse,
    ]
    imports [
        InternalRegex.{ Regex }
    ]

ParseError : [
    Expected U8 [Got] U8 [AtIndex] Nat,
    ExpectedEnd [Got] U8 [AtIndex] Nat,
    EscapeAtEnd,
    UnterminatedBracketGroup [AtIndex] Nat,
]

parse : Str -> Result (Regex U8) ParseError
parse = \input ->
    { newParser: endParser, regex: regexParsed } <-
        input
        |> init
        |> regex
        |> Result.try
    when List.first endParser.input is
        Err ListWasEmpty -> Ok regexParsed
        Ok char -> Err (ExpectedEnd Got char AtIndex endParser.index)



Parser a : {
    input : List a,
    index : Nat,
}

init : Str -> Parser U8
init = \input -> {
    input: Str.toUtf8 input,
    index: 0,
}

# regex <- term '|' regex
#        | term
regex : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
regex = \parser ->
    { newParser: newParser1, regex: termParsed } <- term parser |> Result.try
    if more newParser1 && peek newParser1 == '|' then
        newParser2 <- eat newParser1 '|' |> Result.try
        { newParser: newParser3, regex: regexParsed } <- regex newParser2 |> Result.map
        { newParser: newParser3, regex: OneOf termParsed regexParsed}
    else
        Ok { newParser: newParser1, regex: termParsed }

# term <- factor*
term : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
term = \parser ->
    helper : Parser U8, Regex U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
    helper = \helpParser, acc ->
        if more helpParser && peek helpParser != ')' && peek helpParser != '|' then
            { newParser: helpParser1, regex: factorParsed } <- factor helpParser |> Result.try
            helper helpParser1 (Pair acc factorParsed)
        else
            Ok { newParser: helpParser, regex: acc }
    helper parser Empty

# factor <- base ('*'|'+'|'?')*
factor : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
factor = \parser ->
    { newParser: newParser1, regex: baseParsed } <- base parser |> Result.try

    helper : Parser U8, Regex U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
    helper = \helpParser, acc ->
        if more helpParser && peek helpParser == '*' then
            helpParser1 <- eat helpParser '*' |> Result.try
            helper helpParser1 (Star acc)
        else if more helpParser && peek helpParser == '+' then
            helpParser1 <- eat helpParser '+' |> Result.try
            helper helpParser1 (Pair acc (Star acc))
        else if more helpParser && peek helpParser == '?' then
            helpParser1 <- eat helpParser '?' |> Result.try
            helper helpParser1 (OneOf Empty acc)
        else
            Ok { newParser: helpParser, regex: acc }

    helper newParser1 baseParsed

# base <- char
#       | '\' char
#       | '[' char* ']'
#       | '(' regex ')'
base : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } ParseError
base = \parser ->
    when peek parser is
        '(' ->
            newParser1 <- eat parser '(' |> Result.try
            { newParser: newParser2, regex: regexParsed } <- regex newParser1 |> Result.try
            newParser3 <- eat newParser2 ')' |> Result.map
            { newParser: newParser3, regex: regexParsed }
        '[' ->
            helper : Parser U8, List U8 -> Result { newParser : Parser U8, chars : List U8 } ParseError
            helper = \helpParser, acc ->
                if more helpParser && peek helpParser == ']' then
                    helpParser1 <- eat helpParser ']' |> Result.map
                    { newParser: helpParser1, chars: acc }
                else if more helpParser && peek helpParser == '\\' then
                    helpParser1 <- eat helpParser '\\' |> Result.try
                    { newParser: helpParser2, char } <-
                        next helpParser1
                        |> Result.mapErr \_ -> EscapeAtEnd
                        |> Result.try
                    helper helpParser2 (List.append acc char)
                else
                    { newParser: helpParser1, char } <-
                        next helpParser
                        |> Result.mapErr \_ -> UnterminatedBracketGroup AtIndex (parser.index)
                        |> Result.try
                    helper helpParser1 (List.append acc char)
            newParser1 <- eat parser '[' |> Result.try
            { newParser: newParser2, chars } <- helper newParser1 [] |> Result.map
            { newParser: newParser2, regex: List.walk chars Fail \state, char -> OneOf state (Symbol char) }
        '\\' ->
            newParser1 <- eat parser '\\' |> Result.try
            { newParser: newParser2, char: escaped } <-
                next newParser1
                |> Result.mapErr \_ -> EscapeAtEnd
                |> Result.try
            Ok { newParser: newParser2, regex: Symbol escaped }
        _ ->
            { newParser: newParser1, char } =
                when next parser is
                    Err _ -> crash "unreachable in base"
                    Ok x -> x
            Ok { newParser: newParser1, regex: Symbol char }

eat : Parser U8, U8 -> Result (Parser U8) ParseError
eat = \parser, char ->
    if peek parser == char then
        Ok { parser &
            input: List.dropFirst parser.input,
            index: parser.index + 1,
        }
    else
        Err (Expected char Got (peek parser) AtIndex (parser.index))

next : Parser U8 -> Result { newParser : Parser U8, char : U8 } [NoMoreInput]
next = \parser ->
    char = peek parser
    if char == 0 then
        Err NoMoreInput
    else
        newParser =
            when eat parser char is
                Ok np -> np
                Err _ -> crash "unreachable in next"
        Ok { newParser, char }

peek : Parser U8 -> U8
peek = \parser ->
    when List.first parser.input is
        Err ListWasEmpty -> 0
        Ok char -> char

more : Parser * -> Bool
more = \parser ->
    !(List.isEmpty parser.input)

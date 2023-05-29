interface Parser
    exposes [
        ParseError,
        parse,
    ]
    imports [
        InternalRegex.{ Regex }
    ]

ParseError : InnerParseError [ExpectedEnd [Got] U8 [AtIndex] Nat]
InnerParseError a : [Expected U8 [Got] U8 [AtIndex] Nat]a

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
regex : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
regex = \parser ->
    { newParser: newParser1, regex: termParsed } <- term parser |> Result.try
    if more newParser1 && peek newParser1 == '|' then
        newParser2 <- eat newParser1 '|' |> Result.try
        { newParser: newParser3, regex: regexParsed } <- regex newParser2 |> Result.map
        { newParser: newParser3, regex: OneOf termParsed regexParsed}
    else
        Ok { newParser: newParser1, regex: termParsed }

# term <- factor*
term : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
term = \parser ->
    helper : Parser U8, Regex U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
    helper = \helpParser, acc ->
        if more helpParser && peek helpParser != ')' && peek helpParser != '|' then
            { newParser: helpParser1, regex: factorParsed } <- factor helpParser |> Result.try
            helper helpParser1 (Pair acc factorParsed)
        else
            Ok { newParser: helpParser, regex: acc }
    helper parser Empty

# factor <- base ('*'|'+'|'?')*
factor : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
factor = \parser ->
    { newParser: newParser1, regex: baseParsed } <- base parser |> Result.try

    helper : Parser U8, Regex U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
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
base : Parser U8 -> Result { newParser : Parser U8, regex : Regex U8 } (InnerParseError *)
base = \parser ->
    when peek parser is
        '(' ->
            newParser1 =
                when eat parser '(' is
                    Ok np1 -> np1
                    Err _ -> crash "unreachable"
            { newParser: newParser2, regex: regexParsed } <- regex newParser1 |> Result.try
            newParser3 <- eat newParser2 ')' |> Result.map
            { newParser: newParser3, regex: regexParsed }
        '[' ->
            helper : Parser U8, List U8 -> Result { newParser : Parser U8, chars : List U8 } (InnerParseError *)
            helper = \helpParser, acc ->
                if more helpParser && peek helpParser == ']' then
                    helpParser1 <- eat helpParser ']' |> Result.map
                    { newParser: helpParser1, chars: acc }
                else if more helpParser && peek helpParser == '\\' then
                    helpParser1 <- eat helpParser '\\' |> Result.try
                    { newParser: helpParser2, char } = next helpParser1
                    helper helpParser2 (List.append acc char)
                else
                    { newParser: helpParser1, char } = next helpParser
                    helper helpParser1 (List.append acc char)
            newParser1 <- eat parser '[' |> Result.try
            { newParser: newParser2, chars } <- helper newParser1 [] |> Result.map
            { newParser: newParser2, regex: List.walk chars Fail \state, char -> OneOf state (Symbol char) }
        '\\' ->
            newParser1 =
                when eat parser '\\' is
                    Ok np1 -> np1
                    Err _ -> crash "unreachable"
            { newParser: newParser2, char: escaped } = next newParser1
            Ok { newParser: newParser2, regex: Symbol escaped }
        _ ->
            { newParser: newParser1, char } = next parser
            Ok { newParser: newParser1, regex: Symbol char }

eat : Parser U8, U8 -> Result (Parser U8) (InnerParseError *)
eat = \parser, char ->
    if peek parser == char then
        Ok { parser &
            input: List.dropFirst parser.input,
            index: parser.index + 1,
        }
    else
        Err (Expected char Got (peek parser) AtIndex (parser.index))

next : Parser U8 -> { newParser : Parser U8, char : U8 }
next = \parser ->
    char = peek parser
    newParser =
        when eat parser char is
            Ok np -> np
            Err _ -> crash "unreachable"
    { newParser, char }

peek : Parser U8 -> U8
peek = \parser ->
    when List.first parser.input is
        Err ListWasEmpty -> 0
        Ok char -> char

more : Parser * -> Bool
more = \parser ->
    !(List.isEmpty parser.input)

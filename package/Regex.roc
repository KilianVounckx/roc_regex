interface Regex
    exposes [
        Regex,
        ParseError,
        fromStr,
        matches,
        deriveWord,
        deriveSymbol,
        isNullable,
        simplify,
    ]
    imports [
        Parser,
        InternalRegex,
    ]

Regex a : InternalRegex.Regex a
ParseError : Parser.ParseError

fromStr : Str -> Result (Regex U8) ParseError
fromStr = \input ->
    input
    |> Parser.parse
    |> Result.map simplify

# fromStr
expect
    input = "abc"
    got = fromStr input
    want = Ok (Pair Empty (Pair (Symbol 'a') (Pair (Symbol 'b') (Symbol 'c'))))
    got == want

matches : Regex U8, Str -> Bool
matches = \regex, str ->
    isNullable (deriveWord regex (Str.toUtf8 str))

deriveWord : Regex a, List a -> Regex a | a has Eq
deriveWord = \regex, word ->
    when List.first word is
        Err ListWasEmpty -> regex
        Ok symbol ->
            rest = List.dropFirst word
            regex |> deriveSymbol symbol |> deriveWord rest

deriveSymbol : Regex a, a -> Regex a | a has Eq
deriveSymbol = \regex, symbol ->
    when regex is
        Fail -> Fail
        Empty -> Fail
        Symbol b ->
            if b != symbol then
                Fail
            else
                Empty

        OneOf regex1 regex2 -> OneOf (deriveSymbol regex1 symbol) (deriveSymbol regex2 symbol)
        Pair regex1 regex2 ->
            if isNullable regex1 then
                OneOf (Pair (deriveSymbol regex1 symbol) regex2) (deriveSymbol regex2 symbol)
            else
                Pair (deriveSymbol regex1 symbol) regex2

        Star regex1 ->
            Pair (deriveSymbol regex1 symbol) (Star regex1)

isNullable : Regex a -> Bool
isNullable = \regex ->
    when regex is
        Fail -> Bool.false
        Empty -> Bool.true
        Symbol _ -> Bool.false
        OneOf regex1 regex2 ->
            if isNullable regex1 then
                Bool.true
            else
                isNullable regex2

        Pair regex1 regex2 ->
            if !(isNullable regex1) then
                Bool.false
            else
                isNullable regex2

        Star _ -> Bool.true

simplify : Regex (Num a) -> Regex (Num a) | a has Eq
simplify = \regex ->
    when regex is
        Fail -> Fail
        Empty -> Empty
        Symbol symbol -> Symbol symbol
        OneOf regex1 Fail -> simplify regex1
        OneOf Fail regex2 -> simplify regex2
        OneOf regex1 regex2 ->
            simpleRegex1 = simplify regex1
            simpleRegex2 = simplify regex2
            when compare simpleRegex1 simpleRegex2 is
                Eq -> simpleRegex1
                Lt ->
                    if simpleRegex1 != regex1 || simpleRegex2 != regex2 then
                        simplify (OneOf simpleRegex1 simpleRegex2)
                    else
                        OneOf simpleRegex1 simpleRegex2
                Gt ->
                    if simpleRegex1 != regex1 || simpleRegex2 != regex2 then
                        simplify (OneOf simpleRegex2 simpleRegex1)
                    else
                        OneOf simpleRegex2 simpleRegex1
        Pair _ Fail -> Fail
        Pair Fail _ -> Fail
        Pair regex1 Empty -> simplify regex1
        Pair Empty regex2 -> simplify regex2
        Pair regex1 regex2 ->
            simpleRegex1 = simplify regex1
            simpleRegex2 = simplify regex2
            if simpleRegex1 != regex1 || simpleRegex2 != regex2 then
                simplify (Pair simpleRegex1 simpleRegex2)
            else
                Pair simpleRegex1 simpleRegex2
        Star Fail -> Fail
        Star Empty -> Empty
        Star (Star regex1) -> simplify (Star regex1)
        Star regex1 -> Star (simplify regex1)

# simplify
expect
    regex : Regex U8
    regex =
        ((OneOf Fail Fail) |> Pair (Star (OneOf (Symbol 0) (Symbol 1))))
        |> OneOf ((OneOf Fail Fail) |> Pair (Star (OneOf (Symbol 0) (Symbol 1))))
        |> OneOf ((OneOf Empty Fail) |> Pair (Star (OneOf (Symbol 0) (Symbol 1))))
    want : Regex U8
    want = Star (OneOf (Symbol 0) (Symbol 1))
    got : Regex U8
    got = simplify regex
    got == want

compare : Regex (Num a), Regex (Num a) -> [Eq, Lt, Gt]
compare = \regex1, regex2 ->
    when regex1 is
        Fail ->
            when regex2 is
                Fail -> Eq
                _ -> Lt
        Empty ->
            when regex2 is
                Fail -> Gt
                Empty -> Eq
                _ -> Lt
        Symbol symbol1 ->
            when regex2 is
                Fail | Empty -> Gt
                Symbol symbol2 ->
                    if symbol1 < symbol2 then
                        Lt
                    else if symbol2 > symbol1 then
                        Gt
                    else
                        Eq
                _ -> Lt
        OneOf regex11 regex12 ->
            when regex2 is
                Fail | Empty | Symbol _ -> Gt
                OneOf regex21 regex22 ->
                    when compare regex11 regex21 is
                        Lt -> Lt
                        Gt -> Gt
                        Eq -> compare regex12 regex22
                _ -> Lt
        Pair regex11 regex12 ->
            when regex2 is
                Fail | Empty | Symbol _ | OneOf _ _ -> Gt
                Pair regex21 regex22 ->
                    when compare regex11 regex21 is
                        Lt -> Lt
                        Gt -> Gt
                        Eq -> compare regex12 regex22
                _ -> Lt
        Star regex11 ->
            when regex2 is
                Fail | Empty | Symbol _ | OneOf _ _ | Pair _ _ -> Gt
                Star regex21 -> compare regex11 regex21

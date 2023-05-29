interface InternalRegex
    exposes [
        Regex,
    ]
    imports []

Regex a : [
    Fail,
    Empty,
    Symbol a,
    OneOf (Regex a) (Regex a),
    Pair (Regex a) (Regex a),
    Star (Regex a),
]

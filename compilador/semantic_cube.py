operations = {
    "+": {
        "int": {
            "int": int.__add__,
            "float": float.__add__,
            "str": str.__add__,
            "bool": NotImplemented,
        },
        "float": {
            "int": float.__add__,
            "float": float.__add__,
            "str": str.__add__,
            "bool": NotImplemented,
        },
        "str": {
            "int": str.__add__,
            "float": str.__add__,
            "str": str.__add__,
            "bool": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "str": NotImplemented,
            "bool": NotImplemented,
        },
    },
    "-": {
        "int": {
            "int": int.__sub__,
            "float": float.__sub__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__sub__,
            "float": float.__sub__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
    "*": {
        "int": {
            "int": int.__mul__,
            "float": float.__mul__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__mul__,
            "float": float.__mul__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
    "/": {
        "int": {
            "int": float.__truediv__,
            "float": float.__truediv__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__truediv__,
            "float": float.__truediv__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
    "%": {
        "int": {
            "int": int.__mod__,
            "float": float.__mod__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__mod__,
            "float": float.__mod__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
    ">": {
        "int": {
            "int": int.__gt__,
            "float": float.__gt__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__gt__,
            "float": float.__gt__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__gt__,
        },
    },
    ">=": {
        "int": {
            "int": int.__ge__,
            "float": float.__ge__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__ge__,
            "float": float.__ge__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__ge__,
        },
    },
    "<": {
        "int": {
            "int": int.__lt__,
            "float": float.__lt__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__lt__,
            "float": float.__lt__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__lt__,
        },
    },
    "<=": {
        "int": {
            "int": int.__le__,
            "float": float.__le__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__le__,
            "float": float.__le__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__le__,
        },
    },
    "==": {
        "int": {
            "int": int.__eq__,
            "float": float.__eq__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__eq__,
            "float": float.__eq__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": bool.__eq__,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__eq__,
        },
    },
    "!=": {
        "int": {
            "int": int.__ne__,
            "float": float.__ne__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": float.__ne__,
            "float": float.__ne__,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": bool.__ne__,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": str.__ne__,
        },
    },
    "and": {
        "int": {
            "int": int.__and__,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": bool.__and__,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
    "or": {
        "int": {
            "int": int.__or__,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "float": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
        "bool": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": bool.__or__,
            "str": NotImplemented,
        },
        "str": {
            "int": NotImplemented,
            "float": NotImplemented,
            "bool": NotImplemented,
            "str": NotImplemented,
        },
    },
}

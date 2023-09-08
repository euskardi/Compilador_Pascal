# Define the grammar rules
import ply.lex as lex


grammar = """
    program : statement_list

    statement_list : statement
                   | statement_list statement

    statement : expression_stmt
              | assignment_stmt

    expression_stmt : expression

    assignment_stmt : ID '=' expression

    expression : expression '+' expression
               | expression '*' expression
               | '(' expression ')'
               | NUMBER
               | ID
"""


reserved = {
    "if": "IF",
    "then": "THEN",
    "else": "ELSE",
    "do": "DO",
    "while": "WHILE",
    "for": "FOR",
    "to": "TO",
    "and": "AND",
    "or": "OR",
    "not": "NOT",
    "program": "PROGRAM",
    "begin": "BEGIN",
    "end": "END",
    "var": "VAR",
    "write": "WRITE",
    "integer": "INTEGER",
    "real": "REAL",
    "boolean": "BOOLEAN",
    "character": "CHARACTER",
    "string": "STRING",
    "program": "PROGRAM",
}

# Define the lexer tokens (NUMBERS, '+', '=', and IDS)
tokens = [
    "INT",
    "FLOAT",
    "STR",
    "BOOL",
    "PLUS",
    "MINUS",
    "TIMES",
    "DIVIDE",
    "MOD",
    "INCREMENT",
    "DECREMENT",
    "LPAREN",
    "RPAREN",
    "LKEY",
    "RKEY",
    "EQUALS",
    "SAME",
    "GREATER",
    "GREATER_EQUAL",
    "LESS",
    "LESS_EQUAL",
    "DIFFERENT",
    "ASSIGN",
    "ID",
    "SEMICOLON",
    "COLON",
    "COMMA",
] + list(reserved.values())

t_PLUS = r"\+"
t_MINUS = r"-"
t_TIMES = r"\*"
t_DIVIDE = r"/"
t_MOD = r"%"
t_INCREMENT = r"\+\+"
t_DECREMENT = r"--"
t_LPAREN = r"\("
t_RPAREN = r"\)"
t_LKEY = r"\{"
t_RKEY = r"\}"
t_EQUALS = r"="
t_SAME = r"=="
t_GREATER = r">"
t_LESS = r"<"
t_DIFFERENT = r"<>"
t_GREATER_EQUAL = r">="
t_LESS_EQUAL = r"<="
t_ASSIGN = r":="
t_SEMICOLON = r";"
t_COLON = r":"
t_COMMA = r","
t_ignore = " \t\n"  # Ignore spaces, tabs, and newlines


def t_BOOL(t):
    r"(True|False)"
    t.value = False if t.value == "False" else True
    return t


def t_STR(t):
    r"\"[^\"]+\" "
    t.value = t.value[1:-1]
    return t


def t_FLOAT(t):
    r"[\+|-]?\d+\.\d+"
    t.value = float(t.value)
    return t


def t_INT(t):
    r"[\+|-]?\d+"
    t.value = int(t.value)
    return t


def t_ID(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    t.type = reserved.get(t.value, "ID")
    return t


def t_error(t):
    print(f"Illegal character: {t.value[0]}")
    t.lexer.skip(1)


# Build the lexer
lexer = lex.lex()

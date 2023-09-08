from typing import Any
import ply.yacc as yacc
from lexer import tokens
import semantic_cube

from classes import (
    BinOpNode,
    NumNode,
    AssignNode,
    Quadruplet,
    StatementNode,
    ConditionNode,
    WhileNode,
    ForNode,
    ChangeNode,
    PrintNode,
)


# Define the semantic actions
def p_program(p):
    "program : PROGRAM ID LKEY V BEGIN SEMICOLON statement_list END SEMICOLON RKEY"

    p[0] = p[7]


def p_definition_var(p):
    """
    V : VAR content_var
      |
    """


# Definicion de variables
def p_content_var(p):
    """
    content_var : variable content_var
                | variable
    """


def p_variable_2(p):
    """
    variable : lista COLON INTEGER SEMICOLON
             | lista COLON REAL SEMICOLON
             | lista COLON STRING SEMICOLON
             | lista COLON CHARACTER SEMICOLON
             | lista COLON BOOLEAN SEMICOLON
             | ID COLON INTEGER SEMICOLON
             | ID COLON REAL SEMICOLON
             | ID COLON STRING SEMICOLON
             | ID COLON CHARACTER SEMICOLON
             | ID COLON BOOLEAN SEMICOLON

    """

    if p[3] == "integer":
        for x in p[1]:
            symbol_table[x] = ["integer", 0]
    if p[3] == "real":
        for x in p[1]:
            symbol_table[x] = ["real", 0.0]
    if p[3] == "string":
        symbol_table[p[1]] = ["string", ""]
    if p[3] == "character":
        for x in p[1]:
            symbol_table[x] = ["character", ""]
    if p[3] == "boolean":
        for x in p[1]:
            symbol_table[x] = ["boolean", False]
    pass


def p_lista(p):
    """
    lista : ID
          | ID COMMA lista
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]


def p_statement_list_single(p):
    "statement_list : statement"
    # resolve_symbols(p[1])
    # generate_intermediate_code(p[1])
    p[0] = [p[1]]


def p_statement_list_multiple(p):
    "statement_list : statement_list statement"
    p[0] = p[1] + [p[2]]


def p_statement(p):
    """
    statement : assignment_stmt SEMICOLON
              | expression_stmt SEMICOLON
              | if_stmt
              | while_stmt
              | for_stmt
              | write_stmt SEMICOLON
              | change SEMICOLON
    """
    p[0] = p[1]


def p_write_stmt(p):
    """
    write_stmt : WRITE LPAREN factor RPAREN
               | WRITE LPAREN expression RPAREN
               | WRITE LPAREN expression_string RPAREN
               | WRITE LPAREN expression_bool RPAREN
    """
    p[0] = PrintNode(p[3])


def p_expression_stmt(p):
    """
    expression_stmt : expression
                    | expression_bool
    """
    p[0] = p[1]


def p_assignment_stmt(p):
    """
    assignment_stmt : ID ASSIGN expression
                    | ID ASSIGN expression_bool
                    | ID ASSIGN expression_string
    """
    p[0] = AssignNode(p[1], p[3])


def p_expression_string(p):
    """
    expression_string : expression_string PLUS STR_value
                      | STR_value
    """
    if len(p) == 4:
        p[0] = BinOpNode(p[1], p[2], p[3])
    else:
        p[0] = p[1]


def p_value_string(p):
    """
    STR_value : STR
    """
    p[0] = NumNode(p[1])


def p_if_stmt(p):
    """
    if_stmt : IF LPAREN expression_bool RPAREN THEN LKEY statement_list RKEY
            | IF LPAREN expression_bool RPAREN THEN LKEY statement_list RKEY ELSE LKEY statement_list RKEY
    """
    if len(p) == 9:
        p[0] = ConditionNode(p[3], p[7], "")
    else:
        p[0] = ConditionNode(p[3], p[7], p[11])


def p_while_stmt(p):
    """
    while_stmt : WHILE LPAREN expression_bool RPAREN DO LKEY statement_list RKEY
    """
    p[0] = WhileNode(p[3], p[7])


def p_for_stmt(p):
    """
    for_stmt : FOR LPAREN assignment_stmt SEMICOLON expression_bool SEMICOLON change RPAREN LKEY statement_list RKEY
    """
    p[0] = ForNode(p[3], p[5], p[7], p[10])


def p_expression_boolean_1(p):
    """
    expression_bool : expression_bool AND comparation
                    | expression_bool OR comparation
                    | comparation
    """
    if len(p) == 4:
        p[0] = BinOpNode(p[1], p[2], p[3])
    else:
        p[0] = p[1]


def p_expression_boolean_2(p):
    """
    comparation : expression GREATER expression
                | expression GREATER_EQUAL expression
                | expression LESS expression
                | expression LESS_EQUAL expression
                | expression DIFFERENT expression
                | expression EQUALS expression
                | factor_bool

    """
    if len(p) == 4:
        p[0] = BinOpNode(p[1], p[2], p[3])
    else:
        p[0] = p[1]


def p_expression_boolean_group(p):
    """
    factor_bool : LPAREN expression_bool RPAREN
    """
    p[0] = p[2]


def p_expression_boolean_bool(p):
    """
    factor_bool : BOOL
    """
    p[0] = NumNode(p[1])


def p_expression_1(p):
    """
    expression : expression PLUS term
               | expression MINUS term
               | term
    """
    if len(p) == 4:
        p[0] = BinOpNode(p[1], p[2], p[3])
    else:
        p[0] = p[1]


def p_expression_2(p):
    """
    term : term MOD factor
         | term TIMES factor
         | term DIVIDE factor
         | factor
    """
    if len(p) == 4:
        p[0] = BinOpNode(p[1], p[2], p[3])
    else:
        p[0] = p[1]


def p_expression_group(p):
    "factor : LPAREN expression RPAREN"
    p[0] = p[2]


def p_expression_number(p):
    """
    factor : INT
           | FLOAT
           | ID
    """
    p[0] = NumNode(p[1])


def p_id_change(p):
    """
    change : ID INCREMENT
           | ID DECREMENT
    """
    p[0] = ChangeNode(p[1], p[2])


# Error handling
def p_error(p):
    if p is None:
        raise SyntaxError("Syntax error: Unexpected end of input.")
    else:
        raise SyntaxError("Syntax error at token: " + str(p.value))


# Intermediate code generation and symbol table update
def generate_intermediate_code(node):
    if isinstance(node, list):
        for statement in node:
            generate_intermediate_code(statement)

    elif isinstance(node, ConditionNode):
        label = get_label()
        if node.else_statement != "":
            label2 = get_label()

        actual_temp_val = actual_temp()
        generate_intermediate_code(node.expression)
        quadruplets.append(Quadruplet("GotoF", actual_temp_val, "", label))
        generate_intermediate_code(node.statement)
        if node.else_statement != "":
            quadruplets.append(Quadruplet("Goto", "", "", label2))
            quadruplets.append(Quadruplet("Label", "", "", label))
            generate_intermediate_code(node.else_statement)
            quadruplets.append(Quadruplet("Label", "", "", label2))
        else:
            quadruplets.append(Quadruplet("Label", "", "", label))

    elif isinstance(node, WhileNode):
        label_start = get_label()
        label_end = get_label()
        quadruplets.append(Quadruplet("Label", "", "", label_start))

        actual_temp_val = actual_temp()
        generate_intermediate_code(node.expression)
        quadruplets.append(Quadruplet("GotoF", actual_temp_val, "", label_end))
        generate_intermediate_code(node.statement)
        quadruplets.append(Quadruplet("Goto", "", "", label_start))
        quadruplets.append(Quadruplet("Label", "", "", label_end))

    elif isinstance(node, ForNode):
        label_start = get_label()
        label_end = get_label()
        generate_intermediate_code(node.declaration)
        quadruplets.append(Quadruplet("Label", "", "", label_start))
        actual_temp_val = actual_temp()
        generate_intermediate_code(node.expression)
        quadruplets.append(Quadruplet("GotoF", actual_temp_val, "", label_end))
        generate_intermediate_code(node.statement)
        generate_intermediate_code(node.increment)
        quadruplets.append(Quadruplet("Goto", "", "", label_start))
        quadruplets.append(Quadruplet("Label", "", "", label_end))

    elif isinstance(node, BinOpNode):
        temp = get_temp()
        generate_intermediate_code(node.left)
        generate_intermediate_code(node.right)
        if apply_operation(node.operator, node.left.temp, node.right.temp):
            quadruplets.append(
                Quadruplet(node.operator, node.left.temp, node.right.temp, temp)
            )
        node.temp = temp

    elif isinstance(node, AssignNode):
        generate_intermediate_code(node.expression)

        # verificar que la variable siga siendo entera (ej si declaras una bool pero luego le asignas un int)
        quadruplets.append(Quadruplet(":=", node.expression.temp, None, node.variable))
        # symbol_table[node.variable] = "Number"

    elif isinstance(node, ChangeNode):
        quadruplets.append(Quadruplet(node.increment, "", "", node.value))

    elif isinstance(node, NumNode):
        node.temp = node.value

    elif isinstance(node, PrintNode):
        generate_intermediate_code(node.statement)
        quadruplets.append(Quadruplet("write", "", "", node.statement.temp))


def apply_operation(operator, operand1, operand2):
    type1 = type(operand1).__name__
    type2 = type(operand2).__name__

    if (
        type1 in semantic_cube.operations[operator]
        and type2 in semantic_cube.operations[operator][type1]
    ):
        return True
    else:
        raise TypeError(f"No se puede realizar la operación {type1} {operator} {type2}")


def get_temp():
    global temp_count
    temp = f"T{temp_count}"
    temp_count += 1
    return temp


def actual_temp():
    temp = f"T{temp_count}"
    return temp


def get_label():
    global label_count
    label = f"L{label_count}"
    label_count += 1
    return label


def checking(dictTemporal, value):
    if value in dictTemporal:
        return dictTemporal[value]
    elif value in symbol_table:
        return symbol_table[value][1]
    else:
        return value


def process(quadruplets):
    temporales = {}
    labels = {}

    for x in range(len(quadruplets)):
        if quadruplets[x].op == "Label":
            labels[quadruplets[x].result] = x

    tempi = temp_count
    while tempi > 0:
        tempi = tempi - 1
        temporales["T" + str(tempi)] = None

    y = 0
    while y < len(quadruplets):
        if quadruplets[y].op == ":=":
            symbol_table[quadruplets[y].result][1] = checking(
                temporales, quadruplets[y].arg1
            )
        elif quadruplets[y].op == "++":
            symbol_table[quadruplets[y].result][1] = (
                symbol_table[quadruplets[y].result][1] + 1
            )
        elif quadruplets[y].op == "--":
            symbol_table[quadruplets[y].result][1] = (
                symbol_table[quadruplets[y].result][1] - 1
            )
        elif quadruplets[y].op == "+":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) + checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "-":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) - checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "*":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) * checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "/":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) / checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "and":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) and checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "or":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) or checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == ">":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) > checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "<":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) < checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == ">=":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) >= checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "<=":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) <= checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "<>":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) != checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "=":
            temporales[quadruplets[y].result] = checking(
                temporales, quadruplets[y].arg1
            ) == checking(temporales, quadruplets[y].arg2)
        elif quadruplets[y].op == "Goto":
            y = labels[quadruplets[y].result]
        elif quadruplets[y].op == "GotoF":
            if checking(temporales, quadruplets[y].arg1) == False:
                y = labels[quadruplets[y].result]
        elif quadruplets[y].op == "write":
            print(checking(temporales, quadruplets[y].result))
        y = y + 1


r = "program main35 {var x,y,a : integer; begin; x := 5; y:=4; a:=x+y; if(x > 4) then {x:=y; write(a);} else {write(x);} end;}"
r = 'program main36 {var b,c,z : integer; begin; b :=5; c := -10; z:=2; while(b > c) do {write("sss"); c:=c+z;} end;} '

while True:
    r = input("Codigo: ")
    temp_count = 0
    label_count = 0
    quadruplets = []
    symbol_table = {}

    parser = yacc.yacc()

    result = parser.parse(r)

    print("Parsing successful.")

    print("=== Quadruplets ===\n")
    generate_intermediate_code(result)
    for quadruplet in quadruplets:
        print(quadruplet.op, quadruplet.arg1, quadruplet.arg2, quadruplet.result)

    print("\n=== Output ===\n")

    process(quadruplets)

    print("")

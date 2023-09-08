# Define the AST node classes
class BinOpNode:
    def __init__(self, left, operator, right):
        self.left = left
        self.operator = operator
        self.right = right

    def getLeft(self):
        return self.left

    def getRight(self):
        return self.right


class NumNode:
    def __init__(self, value):
        self.value = value

    def __call__(self):
        return self.value

    def value(self):
        return self.value


class ChangeNode:
    def __init__(self, value, increment):
        self.value = value
        self.increment = increment


class AssignNode:
    def __init__(self, variable, expression):
        self.variable = variable
        self.expression = expression


class StatementNode:
    def __init__(self, statement):
        self.statement = statement


class PrintNode:
    def __init__(self, statement):
        self.statement = statement


class ConditionNode:
    def __init__(self, expression, statement, else_statement):
        self.expression = expression
        self.statement = statement
        self.else_statement = else_statement


class ForNode:
    def __init__(self, declaration, expression, increment, statement):
        self.declaration = declaration
        self.expression = expression
        self.increment = increment
        self.statement = statement


class WhileNode:
    def __init__(self, expression, statement):
        self.expression = expression
        self.statement = statement


class Quadruplet:
    def __init__(self, op, arg1, arg2, result):
        self.op = op
        self.arg1 = arg1
        self.arg2 = arg2
        self.result = result

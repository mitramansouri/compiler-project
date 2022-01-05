import ply.yacc as yacc
from lex import LEXER as obj


class YACC(object):
    tokens = obj.tokens
    _obj = obj()
    lexer = _obj.lexer
    precedence = (
        ('nonassoc', 'GE', 'GT', 'LE', 'LT', 'EQUAL', 'NE',),
        ('left', 'TIMES', 'DIVIDE'),
        ('left', 'PLUS', 'MINUS')
    )

    def __init__(self):
        self.parser = yacc.yacc(module=self)
        self.parseTree = None
        self.three_address_code = None

        self.assign_symbols = ['=']
        self.operation_symbols = ['+', '-', '*', '/',
                                  '==',
                                  '>', '>=', '<', '<=', '!=']
        self.keywords = self._obj.reserved.keys()

        self.symbol_table = {}

        self.tempCount = 0
        self.labelCount = 0

    def temp(self):
        self.tempCount += 1
        return "t%d" % self.tempCount

    def label(self):
        self.labelCount += 1
        return "l%d" % self.labelCount

    def p_stmts(self, p):
        """
        stmts : stmts stmt
        | empty
        """
        if len(p) == 3:
            if not p[1]:
                p[1] = []
            p[0] = p[1] + [p[2]]
            self.parseTree = p[0]

    def p_statement(self, p):
        """
        stmt : assignment
        | for
        | if
        | while
        | expr
        """
        p[0] = p[1]

    def p_if(self, p):
        """
        if : IF expr TWOP LBRACKET stmts RBRACKET elif
        """
        p[0] = list((p[1], p[2], p[5], p[7]))

    def p_elif(self, p):
        """
        elif : ELIF expr TWOP LBRACKET stmts RBRACKET elif
        | else
        """
        if len(p) != 2:
            p[0] = list((p[1], p[2], p[5], p[7]))
        else:
            p[0] = p[1]

    def p_else(self, p):
        """
        else : ELSE TWOP LBRACKET stmts RBRACKET
        | empty
        """
        if len(p) != 2:
            p[0] = list((p[1], p[4]))

    def p_while(self, p):
        """
        while : WHILE expr TWOP LBRACKET stmts RBRACKET
        """
        p[0] = list((p[1], p[2], p[5]))

    def p_for(self, p):
        """
        for : FOR ID IN RANGE LPAREN NUMBER RPAREN TWOP LBRACKET stmts RBRACKET
        """
        p[0] = list((p[1], p[2], p[6], p[10]))

    def p_assignment(self, p):
        """
        assignment : ID ASSIGN expr
        """
        p[0] = list((p[2], p[1], p[3]))

    def p_expr_operator(self, p):
        """
        expr : expr PLUS term
        | expr MINUS term
        | expr GE term
        | expr EQUAL term
        | expr NE term
        | expr LT term
        | expr LE term
        | expr GT term
        | term
        """
        if len(p) != 2:
            p[0] = list([p[2], p[1], p[3]])
        else:
            p[0] = p[1]

    def p_term(self,p):
        """
        term : term TIMES factor
        | term DIVIDE factor
        | factor
        """
        if len(p) != 2:
            p[0] = list([p[2], p[1], p[3]])
        else:
            p[0] = p[1]

    def p_factor(self,p):
        """
        factor : NUMBER
        | ID
        | LPAREN expr RPAREN
        """
        if len(p) != 2:
            p[0] = p[2]
        else:
            p[0] = p[1]

    def p_empty(self, p):
        'empty :'
        p[0] = None

    def parse(self, input_data):
        self.parser.parse(input_data, lexer=self.lexer)
        return self.parseTree

    def get_yacc(self, instruction):
        if type(instruction) != list:
            return "", instruction

        if instruction[0] in self.assign_symbols:
            return self.yacc_assign(instruction)

        elif instruction[0] in self.operation_symbols :
            return self.yacc_operator(instruction)
        elif instruction[0] == 'if':
            return self.yacc_if_elif_else(instruction)
        elif instruction[0] == 'while':
            return self.yacc_while(instruction)
        elif instruction[0] == 'for':
            return self.yacc_for(instruction)
        else:
            raise Exception("Invalid instruction: %s" % str(instruction))

    def yacc_for(self, instruction):
        varOfFor = instruction[1]
        end = instruction[2]
        start = 0
        step = 1
        statements = self.yacc_program(instruction[3])

        startL = self.label()
        endL = self.label()

        tacBody = ""

        if varOfFor not in self.symbol_table:
            tacBody += f"float {varOfFor};\n"
            self.symbol_table[varOfFor] = 'float'

        tacBody += f"{varOfFor} = {start};\n"
        operator = '>=' if step > 0 else '<='

        tacBody += f"{startL}:\n" \
                     f"if ({varOfFor} {operator} {end}) goto {endL};\n" \
                     f"{statements}\n" \
                     f"{varOfFor} += {step};\n" \
                     f"goto {startL};\n" \
                     f"{endL}:\n"
        return tacBody, None

    def yacc_while(self, instruction):
        condition = instruction[1]
        statements = instruction[2]

        condition_yacc_str, condition_root = self.get_yacc(condition)
        statements_yacc_str = self.yacc_program(statements)

        startL = self.label()
        endL = self.label()

        condition_yacc_str_repeat = condition_yacc_str.replace('float ', '')

        tacBody = f"{startL}: \n" \
                    f"if (!{condition_root}) goto {endL};\n" \
                    f"{statements_yacc_str}\n" \
                    f"{condition_yacc_str_repeat}\n" \
                    f"goto {startL};\n" \
                    f"{endL}:\n"

        return condition_yacc_str + tacBody, None

    def yacc_if_elif_else(self, instruction):
        info = []
        _instruction = instruction
        while _instruction:
            keyword = _instruction[0]

            if keyword == 'else':
                info.append({
                    'keyword': 'else',
                    'statements': self.yacc_program(_instruction[1])
                })
                break

            if keyword == 'elif':
                keyword = 'else if'
            info.append({
                'keyword': keyword,
                'condition': self.get_yacc(_instruction[1]),
                'statements': self.yacc_program(_instruction[2])
            })

            _instruction = _instruction[3]

        conditions = ""
        tacBody = ""
        if_done_label = self.label()

        for token in info:
            keyword = token.get('keyword')
            if keyword == 'else':
                tacBody += f"{token.get('statements')}\n"
                continue
            _condition, condition_root = token.get('condition')
            conditions += f"{_condition}\n"

            statements_end = self.label()

            tacBody += f"if (!{condition_root}) goto {statements_end};\n" \
                         f"{token.get('statements')}\n" \
                         f"goto {if_done_label};\n" \
                         f"{statements_end}:\n"

        tacBody += f"{if_done_label}:;\n"
        return conditions + tacBody, None

    def yacc_operator(self, instruction):
        lhs = instruction[1]
        rhs = instruction[2]
        operator = instruction[0]

        lhs_yacc_str, a_root = self.get_yacc(lhs)
        rhs_yacc_str, b_root = self.get_yacc(rhs)

        temp = self.temp()

        _str = lhs_yacc_str + rhs_yacc_str
        _str += f"float {temp} = {a_root} {operator} {b_root};\n"

        return _str, temp

    def yacc_assign(self, instruction):
        lhs = instruction[1]
        rhs = instruction[2]
        operator = instruction[0]

        if lhs in self.symbol_table.keys():
            varType = ''
        else:
            varType = 'float '
            self.symbol_table[lhs] = 'float'

        rhs_str, rhs_root = self.get_yacc(rhs)
        yacc_str = f"{varType}{lhs} {operator} {rhs_root};\n"
        return rhs_str + yacc_str, lhs

    def yacc_program(self, p):
        if not p:
            return "\n"
        returnVal = ""
        for instruction in p:
            _instruction, root = self.get_yacc(instruction)
            returnVal += _instruction

        return returnVal

    def generate_tac(self):
        body = self.yacc_program(self.parseTree)
        ctac = body
        return ctac


if __name__ == "__main__":

    tac = YACC()
    pCode = open('pythonProgram.txt')
    info = pCode.read()
    pCode.close()
    # first we have to parse it and the results are in parse.out
    tac.parse(info)
    cCode = open('cProgram.txt', 'w')
    cCode.write(tac.generate_tac())
    print("Checked out cProgram.txt")
    cCode.close()

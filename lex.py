
import ply.lex as lex

class LEXER(object):
    def __init__(self):
        self.lexer = lex.lex(module=self)
    # copied from site https://www.dabeaz.com/ply/ply.html
    # All lexers must provide a list tokens that defines all of the possible token 
    # names that can be produced by the lexer. This list is always required and is 
    # used to perform a variety of validation checks. The tokens list is also used 
    # by the yacc.py module to identify terminals.
    # List of token names.   This is always required
    tokens = [
        'NUMBER',
        'PLUS',
        'MINUS',
        'TIMES',
        'DIVIDE',
        'LPAREN',
        'RPAREN',
        'ID',
        'ASSIGN',
        'LBRACKET',
        'RBRACKET',
        'LT',
        'GT',
        'LE',
        'GE',
        'NE',
        'EQUAL',

    ]

    # Reserved word
    reserved = {
        'if': 'IF',
        'elif': 'ELIF',
        'else': 'ELSE',
        'while': 'WHILE',
        'for': 'FOR',
        'in': 'IN',
        'range': 'RANGE',
        ':': 'TWOP'
    }

    tokens = tokens + list(reserved.values())

    # IDs
    def t_ID(self,t):
        r':|[a-zA-Z_][a-zA-Z_0-9]*'
        t.type = self.reserved.get(t.value, 'ID')  
        return t

    # Regular expression rules for simple tokens
    t_PLUS = r'\+'
    t_MINUS = r'-'
    t_TIMES = r'\*'
    t_DIVIDE = r'/'
    t_LPAREN = r'\('
    t_RPAREN = r'\)'

    def printList(self):
        print(self.reserved)
    
    # specification of tokens ------------

    # not
    def t_NOT(self,t):
        r'not'
        t.type = self.reserved.get(t.value, 'NOT')
        return t
    # ==
    def t_EQUAL(self,t):
        r'=='
        t.type = self.reserved.get(t.value, 'EQUAL')
        return t
    # >=
    def t_GE(self,t):
        r'>='
        t.type = self.reserved.get(t.value, 'GE')
        return t
    # <=
    def t_LE(self,t):
        r'<='
        t.type = self.reserved.get(t.value, 'LE')
        return t
    # !=
    def t_NE(self,t):
        r'!='
        t.type = self.reserved.get(t.value, 'NE')
        return t
    # less than
    def t_LT(self,t):
        r'<'
        t.type = self.reserved.get(t.value, 'LT')
        return t
    # greater than
    def t_GT(self,t):
        r'>'
        t.type = self.reserved.get(t.value, 'GT')
        return t
    # assign 
    def t_ASSIGN(self,t):
        r'='
        t.type = self.reserved.get(t.value, 'ASSIGN')
        return t
    # L bracket 
    def t_LRACKET(self,t):
        r'{'
        t.type = LEXER.reserved.get(t.value, 'LBRACKET')
        return t
    # R bracket
    def t_RBRACKET(self,t):
        r'}'
        t.type = LEXER.reserved.get(t.value, 'RBRACKET')
        return t

    # A regular expression rule with some action code
    def t_NUMBER(self,t):
        r'\d+'
        t.value = int(t.value)
        return t

    # Define a rule so we can track line numbers
    @staticmethod
    def t_newline(t):
        r'\n+'
        t.lexer.lineno += len(t.value)

    # A string containing ignored characters (spaces and tabs)
    t_ignore = ' \t'

    # Error handling rule
    @staticmethod
    def t_error(t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    def append_multiple_lines(self,file_name, lines_to_append):
        # Open the file in append & read mode ('a+')
        with open(file_name, "a+") as file_object:
            appendEOL = False
            # Move read cursor to the start of file.
            file_object.seek(0)
            # Check if file is not empty
            data = file_object.read(100)
            if len(data) > 0:
                appendEOL = True
            # Iterate over each string in the list
            for line in lines_to_append:
                # If file is not empty then append '\n' before first line for
                # other lines always append '\n' before appending line
                if appendEOL == True:
                    file_object.write("\n")
                else:
                    appendEOL = True
                # Append element at the end of file
                file_object.write(line)



    def test(self):
        # Clear file at first
        with open("tokens.txt", 'r+') as f:
            f.truncate(0)
        with open('pythonProgram.txt', 'r') as file:
            data = file.read().rstrip()

            # Give the lexer some input
        self.lexer.input(data)

        # Tokenize
        TOKENS = []
        while True:
            addedData = ""
            tok = self.lexer.token()
            if not tok:
                break  # No more input
            addedData = tok.type + "  " + str(tok.value)
            TOKENS.append(addedData)

        self.append_multiple_lines("tokens.txt", TOKENS)

lexer = LEXER()
lexer.test()
print("Check out the tokens.txt")







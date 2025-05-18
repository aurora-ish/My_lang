
DIGITS = '0123456789'




TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'


class token:
    def __init__ (self, type, value=None):
        self.type = type
        self.value = value
    def __repr__(self):
        if self.value: return f'{self.type} : {self.value}'
        return f'{self.type}'

######## LEXER#################
###########################

class lexer:
    def __init__(self,fn, text):
        self.fn = fn
        self.text = text
        self.pos = position(-1, 0, -1, fn, text)
        self.current_char = None
        self.advance()

    def advance(self):
        self.pos.advance(self.current_char)
        if self.pos.index < len(self.text):
            self.current_char = self.text[self.pos.index]
        else: self.current_char = None

    def make_tokens(self):
        tokens = []
        while self.current_char!= None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char == '+':
                tokens.append(token(TT_PLUS))
                self.advance()
            elif self.current_char == '-':
                tokens.append(token(TT_MINUS))
                self.advance()
            elif self.current_char == '*':
                tokens.append(token(TT_MUL))
                self.advance()
            elif self.current_char == '/':
                tokens.append(token(TT_DIV))
                self.advance()
            elif self.current_char == '(':
                tokens.append(token(TT_LPAREN))
                self.advance()
            elif self.current_char == ')':
                tokens.append(token(TT_RPAREN))
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            else :
                start = self.pos.copy()

                char = self.current_char
                self.advance()
                return [], illegalchar(start, self.pos, "'"+char+"'")
        return tokens, None
    


    def make_number(self):
        result = ''
        dot = 0
        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if dot == 1: break
                dot += 1
            result += self.current_char
            self.advance()
        if dot == 0:
            return token(TT_INT, int(result))
        else:
            return token(TT_FLOAT, float(result))



class position:
    def __init__(self, index, line, column, fn, ftxt):
        self.index = index
        self.line = line
        self.column = column
        self.fn = fn
        self.ftxt = ftxt
    def advance(self, current_char=None):
        self.index += 1
        self.column += 1
        if current_char == '\n':
            self.line += 1
            self.column = 0
    def copy(self):
        return position(self.index, self.line, self.column, self.fn, self.ftxt)





class error:
    def __init__(self, start, end, name, details):
        self.start = start
        self.end = end
        self.name = name
        self.details = details

    def as_string(self):
        result = f"Error: {self.name}:{self.details}"
        result += f" File {self.start.fn}, line {self.start.line + 1}"
        return result
    


class illegalchar(error):
    def __init__(self,start, end, details):
        super().__init__(start,end,"Illegal character", details)

def run(fn, text):
    # 1) Tokenize
    lex = lexer(fn, text)
    tokens, error = lex.make_tokens()
    if error:
        return None, error

    # 2) Parse
    parser = Parser(tokens)
    ast = parser.parse()
    return ast, None










##############################
#NODES
##############################
class number_node:
    def __init__(self, tok):
        self.tok = tok
    
    def __repr__(self):
        return f"{self.tok}"
class bin_op_node:
    def __init__(self, left, op, right):
        self.left = left
        self.right = right
        self.op= op
    def __repr__(self):
        return f"({self.left}, {self.op}, {self.right})"

#####
#parser
####3333
class Parser:
    def __init__(self, tokens):
        self.tokens    = tokens
        self.tok_idx   = -1
        self.current_tok = None
        self.advance()

    def advance(self):
        self.tok_idx += 1
        if self.tok_idx < len(self.tokens):
            self.current_tok = self.tokens[self.tok_idx]
        return self.current_tok

    def parse(self):
        """
        Entry point for parsing; produces your AST.
        """
        return self.expr()

    def factor(self):
        tok = self.current_tok
        if tok.type in (TT_INT, TT_FLOAT):
            self.advance()
            return number_node(tok)

        # (could also handle unary +/– and parentheses here)

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def bin_op(self, func, ops):
        left = func()                       # call the passed‑in function
        # as long as the *current* token is one of our op types...
        while self.current_tok is not None and self.current_tok.type in ops:
            op = self.current_tok
            self.advance()
            right = func()
            left = bin_op_node(left, op, right)
        return left
        

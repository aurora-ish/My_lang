from string_with_arrows import *
DIGITS = '0123456789'




TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EOF = 'EOF' 


class token:
    def __init__ (self, type, value=None, pos_start= None, pos_end= None):
        self.type = type
        self.value = value
        if pos_start:
            self.pos_start = pos_start
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end
        
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
                tokens.append(token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(token(TT_MINUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '*':
                tokens.append(token(TT_MUL, pos_start=self.pos))
                self.advance()
            elif self.current_char == '/':
                tokens.append(token(TT_DIV, pos_start=self.pos))
                self.advance()
            elif self.current_char == '(':
                tokens.append(token(TT_LPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char == ')':
                tokens.append(token(TT_RPAREN, pos_start=self.pos))
                self.advance()
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            else :
                start = self.pos.copy()

                char = self.current_char
                self.advance()
                return [], illegalchar(start, self.pos, "'"+char+"'")
        tokens.append(token(TT_EOF , pos_start= self.pos))
        return tokens, None
    


    def make_number(self):
        result = ''
        dot = 0
        pos_start = self.pos.copy()

        while self.current_char is not None and (self.current_char.isdigit() or self.current_char == '.'):
            if self.current_char == '.':
                if dot == 1: break
                dot += 1
            result += self.current_char
            self.advance()
        if dot == 0:
            return token(TT_INT, int(result), pos_start, self.pos)
        else:
            return token(TT_FLOAT, float(result), pos_start, self.pos)



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
        result += '\n\n' + string_with_arrows(self.start.ftxt,self.start, self.end )
        return result
    


class illegalchar(error):
    def __init__(self,start, end, details):
        super().__init__(start,end,"Illegal character", details)

class invalidsyntax(error):
    def __init__(self, start, end, details=' '):
        super().__init__(start, end, "Invalid syntax", details)

#############


def run(fn, text):
    # 1) Tokenize
    lex = lexer(fn, text)
    tokens, error = lex.make_tokens()
    if error:
        return None, error

    # 2) Parse
    parser = Parser(tokens)
    ast = parser.parse()
    if error: return None, ast.error

    #run program
    inter = interpreter()
    inter.visit(ast.node)
    return None, None









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
    
class unaryoperations:
    def __init__(self, op, node):
        self.op = op
        self.node = node

    def __repr__(self):
        return f'({self.op}, {self.node})'
    





################
#PARSE RESULT#
#################


class parseresult:
    def __init__(self):
        self.error = None
        self.node = None
     
    def register(self, res):
        if isinstance(res, parseresult):
            if res.error: self.error = res.error
            return res.node
        return res
    def success(self, node):
        self.node= node
        return self
    def failure(self, error):
        self.error = error
        return self 
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
        res= self.expr()
        if not res.error and self.current_tok.type!= TT_EOF:
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end,"unexpected we expected something else -_-"))
        
        """
        Entry point for parsing; produces your AST.
        """
        return res

    def factor(self):
        res = parseresult()
        tok = self.current_tok
        if tok.type in (TT_PLUS, TT_MINUS):
            res.register(self.advance())
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(unaryoperations(tok, factor))


        elif tok.type in (TT_INT, TT_FLOAT):
            res.register(self.advance())
            return res.success(number_node(tok))
        
        elif tok.type == TT_LPAREN:
            res.register(self.advance())
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register(self.advance())
                return res.success(expr)
            else:
                return res.failure(invalidsyntax(tok.pos_start, tok.pos_end, "expected ')', got SOMETHING ELSE EW"))
                

            
        

        # (could also handle unary +/– and parentheses here)4
        return res.failure(invalidsyntax(tok.pos_start, tok.pos_end, "Expected int or float"))

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))

    def expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))

    def bin_op(self, func, ops):
        res = parseresult()

        left = res.register(func())
        if res.error: return res
        
                         # call the passed‑in function
        # as long as the *current* token is one of our op types...
        while self.current_tok is not None and self.current_tok.type in ops:
            op = self.current_tok
            res.register(self.advance())
            right = res.register(func())
            if res.error: return res
            left = bin_op_node(left, op, right)
        return res.success(left)
        


####################
#####INTERPRETER##########
##########
class interpreter:
    def visit(self, node):
        method_name = f"visit_{type(node).__name__}"
        method = getattr(self, method_name, self.no_visit_method)
        return method(node)
    def no_visit_method(self, node):
        raise Exception(f"no method for node type: {type(node).__name__}")
    ####################

    def visit_numnode(self, node):
        print("found num node")
    
    def visit_bin_op_node(self, node):
        print("found bin operator")
        ## WE WERE ONLY VISITING THE ROOT NODE SO number nodes were not visited
        self.visit(node.left)
        self.visit(node.right)
    def visit_un_op_node(self, node):
        print("found unary operator")
        self.visit(node.node)

from string_with_arrows import *
import string
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS+ DIGITS

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_STRING = 'STRING'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_EQ = 'EQ'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_LSQUARE = 'LSQUARE'
TT_RSQUARE = 'RSQUARE'
TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_EOF = 'EOF' 
TT_POW = 'POW'
TT_COMMA = 'COMMA'
TT_ARROW = 'ARROW'


KEYWORDS = [
     'wish', 'nuhuh', 'alsoo', 'or', 'if', 'then', 'elif', 'else', 'for', 'to', 'while', 'step', 'fun'
]

class token:
    def __init__ (self, type, value=None, pos_start= None, pos_end= None):
        self.type = type
        self.value = value
        if pos_start:
            self.pos_start = pos_start.copy()
            self.pos_end = pos_start.copy()
            self.pos_end.advance()
        if pos_end:
            self.pos_end = pos_end
        
    def __repr__(self):
        if self.value: return f'{self.type} : {self.value}'
        return f'{self.type}'
    
    def matches(self, type_, value):
        return self.type == type_ and self.value == value

######## LEXER#################
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
        else: 
            self.current_char = None

    def make_tokens(self):
        tokens = []
        while self.current_char != None:
            if self.current_char in ' \t':
                self.advance()
            elif self.current_char == '+':
                tokens.append(token(TT_PLUS, pos_start=self.pos))
                self.advance()
            elif self.current_char == '-':
                tokens.append(self.make_minus_or_arrow())
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
            elif self.current_char == '[':
                tokens.append(token(TT_LSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == ']':
                tokens.append(token(TT_RSQUARE, pos_start=self.pos))
                self.advance()
            elif self.current_char == '^':
                tokens.append(token(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
            elif self.current_char == '"':
                tokens.append(self.make_string())

            elif self.current_char == '!':
                tok, error= self.make_not_equal()
                if error:
                    return [], error
                tokens.append(tok)
            elif self.current_char == '=':
                tokens.append(self.make_equal())
            elif self.current_char == '<':
                tokens.append(self.make_less_than())
            elif self.current_char == '>':
                tokens.append(self.make_greater_than())
            elif self.current_char == ',':
                tokens.append(token(TT_COMMA, pos_start=self.pos))
                self.advance()
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
    
    def make_string(self):
        string = ''
        pos_start = self.pos.copy()  #  use self.pos not self.pos_start
        escape_char = False
        self.advance()

        escape_chars = { 
        'n': '\n',
        't': '\t',
    }

        while self.current_char != '"' and self.current_char != None:
            if escape_char:
                string += escape_chars.get(self.current_char, self.current_char)  # ✅ Fix 2: use dict.get()
                escape_char = False
            else:
                if self.current_char == '\\':
                    escape_char = True
                else:
                    string += self.current_char
            self.advance()
    
        self.advance()  # Skip closing quote
        return token(TT_STRING, string, pos_start, self.pos)
    


    def make_identifier(self):
        id_str = ''
        pos_start = self.pos.copy()
        while self.current_char != None and self.current_char in LETTERS_DIGITS:
            id_str += self.current_char
            self.advance()
        tok_type = TT_KEYWORD if id_str in KEYWORDS else TT_IDENTIFIER
        return token(tok_type, id_str, pos_start, self.pos)
    
    def make_not_equal(self):
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            return token(TT_NE, pos_start=pos_start, pos_end=self.pos), None
        else:
            return None, ExpectedCharError(pos_start, self.pos, "'='after '!'")
    
    def make_equal(self):
        tok_type = TT_EQ
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = TT_EE
        return token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_less_than(self):
        tok_type = TT_LT
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = TT_LTE
        return token(tok_type, pos_start=pos_start, pos_end=self.pos)
    
    def make_greater_than(self):
        tok_type = TT_GT
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '=':
            self.advance()
            tok_type = TT_GTE
        return token(tok_type, pos_start=pos_start, pos_end=self.pos)
    def make_minus_or_arrow(self):
        tok_type = TT_MINUS
        pos_start = self.pos.copy()
        self.advance()
        if self.current_char == '>':
            self.advance()
            tok_type = TT_ARROW
        return token(tok_type, pos_start=pos_start, pos_end=self.pos)
    

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

class ExpectedCharError(error):
    def __init__(self, pos_start, pos_end, details):
        super().__init__(pos_start, pos_end, "Expected Char", details)

class illegalchar(error):
    def __init__(self,start, end, details):
        super().__init__(start,end,"Illegal character", details)

class invalidsyntax(error):
    def __init__(self, start, end, details=' '):
        super().__init__(start, end, "Invalid syntax", details)

class rterror(error):
    def __init__(self, start, end, details, context):
        super().__init__(start, end, "runtime error", details)
        self.context= context
        
    def as_string(self):
        result = self.generate_traceback()
        result += f"Error: runtime error: {self.details}\n"
        result += string_with_arrows(self.start.ftxt,self.start, self.end )
        return result
        
    def generate_traceback(self):
        result =''
        pos = self.start
        ctx = self.context
        while ctx:
            result = f'File {pos.fn}, line{str(pos.line+1) }, in {ctx.display_name}\n'+result
            pos = ctx.parent_entry_pos
            ctx = ctx.parent
        return result
'''
kriti
pandey
'''


####################
#runtime result
####################
class rtresult:
    def __init__(self):
        self.value = None
        self.error = None
    
    def register(self, res):
        if res and res.error: 
            self.error = res.error
        return res.value if res else None
        
    def success(self, value):
        self.value = value
        return self
        
    def failure(self, error):
        self.error = error
        return self

##############################
#NODES
##############################
class number_node:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
        
    def __repr__(self):
        return f"{self.tok}"

class string_node:
    def __init__(self, tok):
        self.tok = tok
        self.pos_start = self.tok.pos_start
        self.pos_end = self.tok.pos_end
        
    def __repr__(self):
        return f"{self.tok}"

class list_node:
    def __init__(self, element_nodes, pos_start, pos_end):
        self.element_nodes = element_nodes
        self.pos_start = pos_start
        self.pos_end = pos_end



class bin_op_node:
    def __init__(self, left, op, right):
        self.left = left
        self.right = right
        self.op= op
        self.pos_start = self.left.pos_start
        self.pos_end = self.right.pos_end
        
    def __repr__(self):
        return f"({self.left}, {self.op}, {self.right})"
    
class IfNode:
    def __init__(self, cases, else_case):
        self.cases = cases
        self.else_case = else_case
        self.pos_start = self.cases[0][0].pos_start
        self.pos_end = (self.else_case.pos_end if self.else_case else self.cases[len(self.cases)-1][0].pos_end)

class forNode:
    def __init__(self, var_name, start, end,step, body):
        self.var_name = var_name
        self.start = start
        self.end = end
        self.step = step
        self.body = body
        self.pos_start = self.var_name.pos_start
        self.pos_end = self.body.pos_end

class whileNode:
    def __init__(self, condition, body):
        self.condition = condition
        self.body = body
        self.pos_start = self.condition.pos_start
        self.pos_end = self.body.pos_end

class unaryoperations:
    def __init__(self, op, node):
        self.op = op
        self.node = node
        self.pos_start = self.op.pos_start
        self.pos_end = self.node.pos_end

    def __repr__(self):
        return f'({self.op}, {self.node})'

class varaccess:
    def __init__(self, var_name_tok):
        self.var_name_tok = var_name_tok
        self.pos_start = self.var_name_tok.pos_start 
        self.pos_end = self.var_name_tok.pos_end
    
class varassign:
    def __init__(self, var_name_tok, value):
        self.var_name_tok = var_name_tok
        self.value = value
        self.pos_start = self.var_name_tok.pos_start
        self.pos_end = self.value.pos_end

class funcdefnode:
    def __init__(self, var_name_tok, arg_name_toks, body_node ):
        self.var_name_tok = var_name_tok
        self.arg_name_toks = arg_name_toks  # FIXED: Changed from arg_name_token to arg_name_toks
        self.body_node = body_node

        if self.var_name_tok:
            self.pos_start = self.var_name_tok.pos_start
        elif len(self.arg_name_toks) > 0:  # FIXED: Changed from arg_name_token to arg_name_toks
            self.pos_start = self.arg_name_toks[0].pos_start
        else:
            self.pos_start = self.body_node.pos_start
        
        self.pos_end = self.body_node.pos_end

class callnode:
    def __init__(self, node_to_call, arg_nodes):
        self.node_to_call = node_to_call
        self.arg_nodes = arg_nodes
        self.pos_start = self.node_to_call.pos_start

        if len(self.arg_nodes)>0:
            self.pos_end = self.arg_nodes[-1].pos_end
        else:
            self.pos_end = self.node_to_call.pos_end



        





################
#PARSE RESULT#
#################
class parseresult:
    def __init__(self):
        self.error = None
        self.node = None
        self.advance_count = 0
     
    def register_advancement(self):
        self.advance_count += 1
        
    def register(self, res):
        self.advance_count += res.advance_count
        if res.error: self.error = res.error
        return res.node
        
    def success(self, node):
        self.node= node
        return self
        
    def failure(self, error):
        if self.advance_count == 0:
            self.error = error
        return self 

#####
#parser
####
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
        return res
    
    def list_expr(self):
        res = parseresult()
        element_nodes = []
        pos_start = self.current_tok.pos_start.copy()
        if self.current_tok.type != TT_LSQUARE:
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end,"expected '['"))
        res.register_advancement()
        self.advance()

        if self.current_tok == TT_RSQUARE:
            res.register_advancement()
            self.advance()
        else:
            element_nodes.append(res.register(self.expr()))
            if res.error: 
                return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected var or int or float or identifier + - pr ( or ), if for while [ ] "))
        
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                element_nodes.append(res.register(self.expr()))
                if res.error: return res
        
            if self.current_tok.type != TT_RSQUARE:
                return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, 
                                            "Expected ')' or comma"))
            res.register_advancement()
            self.advance()
        
        return res.success(list_node(element_nodes, pos_start, self.current_tok.pos_end.copy()))

                
            

    def if_expr(self):
        res = parseresult()
        cases = []
        else_case = None

        # Check for 'if' keyword (lowercase)
        if not self.current_tok.matches(TT_KEYWORD, 'if'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'if'"
            ))

        res.register_advancement()
        self.advance()

        # Parse the condition after 'if'
        condition = res.register(self.expr())
        if res.error:
            return res

        # Check for 'then' keyword
        if not self.current_tok.matches(TT_KEYWORD, 'then'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                "Expected 'then'"
            ))

        res.register_advancement()
        self.advance()

        # Parse the expression after 'then'
        expr = res.register(self.expr())
        if res.error:
            return res

        cases.append((condition, expr))

        # Handle 'elif' clauses
        while self.current_tok.matches(TT_KEYWORD, 'elif'):
            res.register_advancement()
            self.advance()

            condition = res.register(self.expr())
            if res.error:
                return res

            if not self.current_tok.matches(TT_KEYWORD, 'then'):
                return res.failure(invalidsyntax(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    "Expected 'then'"
                ))

            res.register_advancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error:
                return res

            cases.append((condition, expr))

        # Handle optional 'else' clause
        if self.current_tok.matches(TT_KEYWORD, 'else'):
            res.register_advancement()
            self.advance()

            else_case = res.register(self.expr())
            if res.error:
                return res

        return res.success(IfNode(cases, else_case))
    
    def for_expr(self):
        res = parseresult()

        if not self.current_tok.matches(TT_KEYWORD, 'for'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'for'"
        ))

        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_IDENTIFIER:
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
            f"Expected identifier"
        ))

        var_name = self.current_tok
        res.register_advancement()
        self.advance()

        if self.current_tok.type != TT_EQ:
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
            f"Expected '='"
        ))

        res.register_advancement()
        self.advance()

        start_value = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, 'to'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
            f"Expected 'to'"
        ))

        res.register_advancement()  # Fixed: missing register_advancement
        self.advance()

        end_value = res.register(self.expr())
        if res.error:
            return res

        if self.current_tok.matches(TT_KEYWORD, 'step'):
            res.register_advancement()
            self.advance()

            step_value = res.register(self.expr())
            if res.error:
                return res
        else:
            step_value = None

        if not self.current_tok.matches(TT_KEYWORD, 'then'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected 'then'"
        ))

        res.register_advancement()
        self.advance()

        body = res.register(self.expr())
        if res.error:
            return res

        return res.success(forNode(var_name, start_value, end_value, step_value, body))

    def while_expr(self):
        res = parseresult()

        if not self.current_tok.matches(TT_KEYWORD, 'while'):
            return res.failure(invalidsyntax(
            self.current_tok.pos_start, self.current_tok.pos_end,
            f"Expected 'while'"
        ))

        res.register_advancement()
        self.advance()

        condition = res.register(self.expr())
        if res.error:
            return res

        if not self.current_tok.matches(TT_KEYWORD, 'then'):
            return res.failure(invalidsyntax(
            self.current_tok.pos_start, self.current_tok.pos_end,
            f"Expected 'then'"
        ))

        res.register_advancement()
        self.advance()

        body = res.register(self.expr())
        if res.error:
           return res

        return res.success(whileNode(condition, body))
    
    def func_def(self):
        res = parseresult()
        if not self.current_tok.matches(TT_KEYWORD, 'fun'):
            return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                             f"expected 'fun'"))
        res.register_advancement()
        self.advance()

        if self.current_tok.type == TT_IDENTIFIER:
            var_name_tok = self.current_tok
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_LPAREN:
                return res.failure(invalidsyntax(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected '(' after function name"))
        else:
            var_name_tok = None
            res.register_advancement()
            self.advance()
            if self.current_tok.type != TT_LPAREN:
                return res.failure(invalidsyntax(
                self.current_tok.pos_start, self.current_tok.pos_end,
                f"Expected '(' or fun name"))
            
        res.register_advancement()
        self.advance()    
        arg_name_toks = []
        if self.current_tok.type == TT_IDENTIFIER:
            arg_name_toks.append(self.current_tok)
            res.register_advancement()
            self.advance()
            while self.current_tok.type == TT_COMMA:
                res.register_advancement()
                self.advance()
                if self.current_tok.type!= TT_IDENTIFIER:
                    return res.failure(invalidsyntax(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        f"Expected identifier after comma"))
                arg_name_toks.append(self.current_tok)
                res.register_advancement()
                self.advance()

            if self.current_tok.type!= TT_RPAREN:
                return res.failure(invalidsyntax(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected ')' or comma"))
        else:
            if self.current_tok.type!= TT_RPAREN:
                return res.failure(invalidsyntax(
                        self.current_tok.pos_start, self.current_tok.pos_end,
                        f"Expected ')' or identifier"))
        res.register_advancement()
        self.advance()  

        if self.current_tok.type!= TT_ARROW:
            return res.failure(invalidsyntax(
                    self.current_tok.pos_start, self.current_tok.pos_end,
                    f"Expected '->'"))
        res.register_advancement()
        self.advance()
        node_to_return = res.register(self.expr())
        if res.error: return res
        return res.success(funcdefnode(
            var_name_tok,
            arg_name_toks,
            node_to_return

            ))

                     
                                        
    def call(self):
        res = parseresult()
        atom = res.register(self.atom())
        if res.error: return res
        if self.current_tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            arg_nodes = []
            
            if self.current_tok.type== TT_RPAREN:
                res.register_advancement()
                self.advance()
            else:
                arg_nodes.append(res.register(self.expr()))
                if res.error: 
                    return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected var or int or float or identifier + - pr ( or ), if for while fun OR RIGHT PARANTHESIS SINCE THIS IS A FUNC "))
            
                while self.current_tok.type == TT_COMMA:
                    res.register_advancement()
                    self.advance()
                    arg_nodes.append(res.register(self.expr()))
                    if res.error: return res
            
                if self.current_tok.type != TT_RPAREN:
                    return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, 
                                                "Expected ')' or comma"))
                res.register_advancement()
                self.advance()
            return res.success(callnode(atom, arg_nodes))
        return res.success(atom)

    
    






 
    def atom(self):
        res = parseresult()
        tok = self.current_tok
        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(number_node(tok))
        elif tok.type in TT_STRING:
            res.register_advancement()
            self.advance()
            return res.success(string_node(tok))
        elif tok.type == TT_IDENTIFIER:
            res.register_advancement()
            self.advance()
            return res.success(varaccess(tok))
        elif tok.type == TT_LPAREN:
            res.register_advancement()
            self.advance()
            expr = res.register(self.expr())
            if res.error: return res
            if self.current_tok.type == TT_RPAREN:
                res.register_advancement()
                self.advance()
                return res.success(expr)
            else:
                return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected ')', got SOMETHING ELSE EW"))
        
        elif tok.type == TT_LSQUARE:
            list_expr = res.register(self.list_expr())
            if res.error: return res
            return res.success(list_expr)

        elif tok.matches(TT_KEYWORD, 'if'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)
        
        elif tok.matches(TT_KEYWORD, 'for'):
            for_expr = res.register(self.for_expr())
            if res.error: return res
            return res.success(for_expr)
        
        elif tok.matches(TT_KEYWORD, 'while'):
            while_expr = res.register(self.while_expr())
            if res.error: return res
            return res.success(while_expr)
        
        elif tok.matches(TT_KEYWORD, 'fun'):
            func_def = res.register(self.func_def())
            if res.error: return res
            return res.success(func_def)

        return res.failure(invalidsyntax(tok.pos_start, tok.pos_end, "Expected int or float or + or - or ( "))

    def power(self):
        return self.bin_op(self.call, (TT_POW, ), self.factor)

    def factor(self):
        res = parseresult()
        tok = self.current_tok
        if tok.type in (TT_PLUS, TT_MINUS):
            res.register_advancement()
            self.advance()
            factor = res.register(self.factor())
            if res.error: return res
            return res.success(unaryoperations(tok, factor))
        return self.power()

    def term(self):
        return self.bin_op(self.factor, (TT_MUL, TT_DIV))
    
    def arith_expr(self):
        return self.bin_op(self.term, (TT_PLUS, TT_MINUS))
    
    def comp_expr(self):
        res = parseresult()
        if self.current_tok.matches(TT_KEYWORD, "nuhuh"):
            op_tok= self.current_tok
            res.register_advancement()
            self.advance()
            node = res.register(self.comp_expr())
            if res.error: return res
            return res.success(unaryoperations(op_tok, node))
        node = res.register(self.bin_op(self.arith_expr, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
        if res.error: 
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "Expected int , float , +,   - , '(', 'nuhuh "))
        return res.success(node)
    
    def expr(self):
        
        res = parseresult()

        if self.current_tok.matches(TT_KEYWORD, 'wish'):
            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_IDENTIFIER:
                return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected identifier"))
            var_name = self.current_tok

            res.register_advancement()
            self.advance()

            if self.current_tok.type != TT_EQ:
                return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected '='"))
            res.register_advancement()
            self.advance()

            expr = res.register(self.expr())
            if res.error: return res
            return res.success(varassign(var_name, expr))
        
        node = res.register(self.bin_op(self.comp_expr, ((TT_KEYWORD, "alsoo"), (TT_KEYWORD, "or"))))
        if res.error: 
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected var or int or float or identifier + - pr ( or ), if for while fun "))
        return res.success(node)

    def bin_op(self, func_a, ops, func_b= None):
        if func_b == None:
            func_b = func_a
        res = parseresult()
    
        left = res.register(func_a())
        if res.error: return res
        
        while self.current_tok is not None and (self.current_tok.type in ops or (self.current_tok.type, self.current_tok.value) in ops):
            op = self.current_tok
            res.register_advancement()
            self.advance()
            right = res.register(func_b())
            if res.error: return res
            left = bin_op_node(left, op, right)
        return res.success(left)

#########################
#####VALUES
#########################
class value:
    def __init__(self):
        self.set_pos()
        self.set_context()
    
    def set_context(self, context = None):
        self.context = context
        return self
    
    def set_pos(self, pos_start= None, pos_end= None):
        self.pos_start= pos_start
        self.pos_end = pos_end
        return self
    
    def added_to(self, other):
        return None, self.illegal_operation(other)
    
    def subbed_by(self, other):
        return None, self.illegal_operation(other)
    
    def multed_by(self, other):
        return None, self.illegal_operation(other)
    
    def divd_by(self, other):
        return None, self.illegal_operation(other)
    
    def powed_by(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_eq(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_ne(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_lt(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_gt(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_lte(self, other):
        return None, self.illegal_operation(other)
    
    def get_comparision_gte(self, other):
        return None, self.illegal_operation(other)
    
    def anded_by(self, other):
        return None, self.illegal_operation(other)
    
    def ored_by(self, other):
        return None, self.illegal_operation(other)
    
    def notted(self):
        return None, self.illegal_operation()
    
    def is_true(self):
        return False
    
    def copy(self):
        raise Exception('no copy method defined')
    
    def execute(self, args):
        return None, self.illegal_operation()
    
    def illegal_operation(self, other=None):
        if not other: other = self
        return rterror(
			self.pos_start, other.pos_end,
			'Illegal operation',
			self.context
		)
    def copy(self):
        string_copy = string(self.value) 
        string_copy.set_context(self.context)
        string_copy.set_pos(self.pos_start, self.pos_end)
        return string_copy
    
    def __repr__(self):
        return f'"{self.value}"'
    

class number(value):
    def __init__(self, value):
        self.value = value
        self.set_pos()
        self.set_context()
        
    def set_context(self, context = None):
        self.context = context
        return self
    
    def set_pos(self, pos_start= None, pos_end= None):
        self.pos_start= pos_start
        self.pos_end = pos_end
        return self
    
        
    def added_to(self, other):
        if isinstance(other, number):
            return number(self.value+other.value).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def subbed_by(self, other):
        if isinstance(other, number):
            return number(self.value-other.value).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)   

    def multed_by(self, other):
        if isinstance(other, number):
            return number(self.value*other.value).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)   


    def powed_by(self, other):
        if isinstance(other, number):
            return number(self.value**other.value).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)    

    def dived_by(self, other):
        if isinstance(other, number):
            if other.value == 0:
                return None, rterror(other.pos_start, other.pos_end,  'Div by 0 error omg thts so stupid', self.context )
            return number(self.value/other.value).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def get_comparision_eq(self, other):
        if isinstance(other, number):
            return number(int(self.value == other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def get_comparision_ne(self, other):
        if isinstance(other, number):
            return number(int(self.value != other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def get_comparision_lt(self, other):
        if isinstance(other, number):
            return number(int(self.value < other.value)).set_context(self.context), None
    
    def get_comparision_gt(self, other):
        if isinstance(other, number):
            return number(int(self.value > other.value)).set_context(self.context), None

    def get_comparision_lte(self, other):
        if isinstance(other, number):
            return number(int(self.value <= other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def get_comparision_gte(self, other):
        if isinstance(other, number):
            return number(int(self.value >= other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def anded_by(self, other):
        if isinstance(other, number):
            return number(int(self.value and other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def ored_by(self, other):
        if isinstance(other, number):
            return number(int(self.value or other.value)).set_context(self.context), None
        else: return None, value.illegal_operation(self.pos_start, self.pos_end)

    def notted(self):
        return number(1 if self.value==0 else 0).set_context(self.context), None

    def is_true(self):
        return self.value != 0

    def __repr__(self):
        return str(self.value)

class string(value):
    def __init__(self, value):
        super().__init__()
        self.value = value

    def added_to(self, other):
        if isinstance(other, string):
            return string(self.value + other.value).set_context(self.context), None
        else:
            return None, value.illegal_operation(self, other)
        
    def multed_by(self, other):
        if isinstance(other, number):
            return string(self.value * other.value).set_context(self.context), None
        else:
            return None, value.illegal_operation(self, other)
    def is_true(self):
        return len(self.value) > 0
    

class list(value):
    def __init__(self, elements):
        super().__init__()
        self.elements = elements
    def added_to(self, other):
        new_list = self.copy()
        new_list.elements.append(other)
        return new_list, None
    
    def multed_by(self, other):
        if isinstance(other, list):
            new_list = self.copy()
            new_list.elements.extend(other.elements)
            return new_list, None
        else:
            return None, value.illegal_operation(self, other)
        
    def subbed_by(self, other):
        if isinstance(other, number):
            new_list = self.copy()
            try:
                new_list.elements.pop(other.value)
                return new_list, None
            except:
                return None, rterror(
                    other.pos_start, other.pos_end, 'element at this index not found', self.context
                )
        else:
            return None, value.illegal_operation(self, other)
    

    def dived_by(self, other):
        if isinstance(other, number):
            try:
                return self.elements[other.value], None
            except:
                return None, rterror(
                    other.pos_start, other.pos_end, 'element at this index not retrieved', self.context
                )
        else:
            return None, value.illegal_operation(self, other)
        
    def copy(self):
        copy = list(self.elements[:])
        copy.set_pos(self.pos_start, self.pos_end)
        copy.set_context(self.context)
        return copy
    
    def __repr__(self):
        return f'[{", ".join([str(x) for x in self.elements])}]'
    
        
        

class function(value):
    def __init__(self, name, body_node, arg_names):
        super().__init__()
        self.name = name or "<anonymous>"
        self.body_node = body_node
        self.arg_names = arg_names

    def execute(self, args):
        res = rtresult()
        inter = interpreter()
        new_context = context(self.name, self.context, self.pos_start)
        new_context.symbol_table = symbol_table(new_context.parent.symbol_table)

        if len(args) > len(self.arg_names):
            return res.failure(rterror(
				self.pos_start, self.pos_end,
				f"{len(args) - len(self.arg_names)} too many args passed into '{self.name}'",
				self.context
			))
        
        if len(args) < len(self.arg_names):
            return res.failure(rterror(
				self.pos_start, self.pos_end,
				f"{len(self.arg_names) - len(args)} too few args passed into '{self.name}'",
				self.context
			))
        
        for i in range(len(args)):
            arg_name = self.arg_names[i]
            arg_value = args[i]
            arg_value.set_context(new_context)
            new_context.symbol_table.set(arg_name, arg_value)
        
        value = res.register(inter.visit(self.body_node, new_context))
        if res.error: return res
        return res.success(value)
    
    def copy(self):
        copy = function(self.name, self.body_node, self.arg_names)
        copy.set_context(self.context)
        copy.set_pos(self.pos_start, self.pos_end)
        return copy
    def __repr__(self):
        return f"<function {self.name}>"



#################
##CONTEXT#######
#################
class context:
    def __init__(self, display_name, parent= None, parent_entry_pos= None):
        self.display_name = display_name
        self.parent = parent
        self.parent_entry_pos = parent_entry_pos
        self.symbol_table = None

######################
####SYMBOL TABLE######
######################
class symbol_table:
    def __init__(self, parent = None):
        self.symbols = {}
        self.parent = None
        
    def get(self, name):
        value = self.symbols.get(name, None)
        if value == None and self.parent:
            return self.parent.get(name)
        return value
        
    def set(self, name, value):
        self.symbols[name] = value
        
    def remove(self, name):
        del self.symbols[name]

####################
#####INTERPRETER##########
##########
class interpreter:
    def visit(self, node, context):
        method_name = f"visit_{type(node).__name__}"
        method = getattr(self, method_name, self.no_visit_method)
        return method(node, context)
        
    def no_visit_method(self, node, context):
        raise Exception(f"no method for node type: {type(node).__name__}")

    def visit_number_node(self, node, context):
        return rtresult().success(number(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_string_node(self, node, context):
        return rtresult().success(string(node.tok.value).set_context(context).set_pos(node.pos_start, node.pos_end))

    def visit_list_node(self, node, context):
        res = rtresult()
        elements = []
        for element_node in node.element_nodes:
            elements.append(res.register(self.visit(element_node, context))) 
            if res.error: return res  
    
        return res.success(list(elements).set_context(context).set_pos(node.pos_start, node.pos_end))


    def visit_varaccess(self, node, context):
        res = rtresult()
        var_name = node.var_name_tok.value
        value = context.symbol_table.get(var_name)

        if not value:
            return res.failure(rterror(node.pos_start, node.pos_end, f"unknown wish '{var_name}'", context))
        
        value = value.set_pos(node.pos_start, node.pos_end).set_context(context)
        return res.success(value)

    def visit_varassign(self, node, context):
        res = rtresult()
        var_name = node.var_name_tok.value
        value = res.register(self.visit(node.value, context))
        if res.error: return res
        context.symbol_table.set(var_name, value)
        return res.success(value)

    def visit_bin_op_node(self, node, context):
        res = rtresult()
        left = res.register(self.visit(node.left, context))
        if res.error: return res
        right = res.register(self.visit(node.right, context))
        if res.error: return res
        
        if node.op.type == TT_PLUS:
            result, error = left.added_to(right)
        elif node.op.type == TT_MINUS:
            result, error = left.subbed_by(right)
        elif node.op.type == TT_MUL:
            result, error = left.multed_by(right)
        elif node.op.type == TT_DIV:
            result, error = left.dived_by(right)
        elif node.op.type == TT_POW:
            result, error = left.powed_by(right)
        elif node.op.type == TT_EE:
            result, error = left.get_comparision_eq(right)
        elif node.op.type == TT_NE:
            result, error = left.get_comparision_ne(right)
        elif node.op.type == TT_LT:
            result, error = left.get_comparision_lt(right)
        elif node.op.type == TT_GT:
            result, error = left.get_comparision_gt(right)
        elif node.op.type == TT_LTE:
            result, error = left.get_comparision_lte(right) 
        elif node.op.type == TT_GTE:
            result, error = left.get_comparision_gte(right)
        elif node.op.matches(TT_KEYWORD, 'alsoo'):
            result, error = left.anded_by(right)
        elif node.op.matches(TT_KEYWORD, 'or'):
            result, error = left.ored_by(right)

        if error:
            return res.failure(error)
        else:
            return res.success(result.set_pos(node.pos_start, node.pos_end))
            
    def visit_IfNode(self, node, context):
        res = rtresult()
        for condition, expr in node.cases:
            condition_value = res.register(self.visit(condition, context))
            if res.error: return res
            if condition_value.is_true():  # Fixed: added parentheses to call the method
                expr_value = res.register(self.visit(expr, context))
                if res.error: return res
                return res.success(expr_value)
            
        if node.else_case:
            else_value = res.register(self.visit(node.else_case, context))
            if res.error: return res
            return res.success(else_value)
        return res.success(number(0))  # Fixed: return number(0) instead of None
    
    def visit_forNode(self, node, context):
        res = rtresult()
        elements = []
        start_value = res.register(self.visit(node.start, context))
        if res.error: return res
        end_value = res.register(self.visit(node.end, context))
        if res.error: return res
    
        if node.step:
            step_value = res.register(self.visit(node.step, context))
            if res.error: return res
        else:
            step_value = number(1)
    
        i = start_value.value
    
        if step_value.value >= 0:
            condition = lambda: i <= end_value.value
        else:
            condition = lambda: i >= end_value.value
    
        while condition():
            context.symbol_table.set(node.var_name.value, number(i))
            elements.append(res.register(self.visit(node.body, context)))

            if res.error: return res
            i += step_value.value  # Move this AFTER body execution
        
        return res.success(list(elements).set_context(context).set_pos(node.pos_start, node.pos_end))
    def visit_whileNode(self, node, context):
        res = rtresult()
        elements = []
        while True:
            condition = res.register(self.visit(node.condition, context))
            if res.error: return res
            if not condition.is_true():
                break
            elements.append(res.register(self.visit(node.body, context)))
            if res.error: return res
        return res.success(list(elements).set_context(context).set_pos(node.pos_start, node.pos_end))  

    def visit_unaryoperations(self, node, context):
        res = rtresult()
        num = res.register(self.visit(node.node, context))
        if res.error: return res
        
        error = None
        if node.op.type == TT_MINUS:
            num, error = num.multed_by(number(-1))
        elif node.op.matches(TT_KEYWORD, 'nuhuh'):
            num, error = num.notted()
            
        if error:
            return res.failure(error)
        else:
            return res.success(num.set_pos(node.pos_start, node.pos_end))


    def visit_funcdefnode(self, node, context):
        res = rtresult()
        func_name = node.var_name_tok.value
        body_node = node.body_node
        arg_names = [arg_name.value for arg_name in node.arg_name_toks]
        func_value = function(func_name, body_node, arg_names).set_context(context).set_pos(node.pos_start, node.pos_end)

        if node.var_name_tok:
            context.symbol_table.set(func_name, func_value)
        
        return res.success(func_value)
    
    def visit_callnode(self, node, context):
        res = rtresult()
        args = []
        value_to_call = res.register(self.visit(node.node_to_call, context))
        if res.error: return res
        value_to_call = value_to_call.copy().set_pos(node.pos_start, node.pos_end)

        for arg_node in node.arg_nodes:
            args.append(res.register(self.visit(arg_node, context)))
            if res.error: return res

        return_value = res.register(value_to_call.execute(args))
        if res.error: return res
        return res.success(return_value)
    

#############
###RUN#######
#############

global_symbol_table = symbol_table()
global_symbol_table.set("null", number(0))
global_symbol_table.set("true", number(1))
global_symbol_table.set("false", number(0))

def run(fn, text):
    # 1) tokenize
    lex = lexer(fn, text)
    tokens, error = lex.make_tokens()
    if error:
        return None, error

    # 2) Parse
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error:
        return None, ast.error

    # 3) Interpret
    inter = interpreter()
    cont = context('<program>')
    cont.symbol_table = global_symbol_table
    result = inter.visit(ast.node, cont)

    return result.value, result.error

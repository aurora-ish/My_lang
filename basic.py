from string_with_arrows import *
import string
DIGITS = '0123456789'
LETTERS = string.ascii_letters
LETTERS_DIGITS = LETTERS+ DIGITS

TT_INT = 'INT'
TT_FLOAT = 'FLOAT'
TT_IDENTIFIER = 'IDENTIFIER'
TT_KEYWORD = 'KEYWORD'
TT_EQ = 'EQ'
TT_PLUS = 'PLUS'
TT_MINUS = 'MINUS'
TT_MUL = 'MUL'
TT_DIV = 'DIV'
TT_LPAREN = 'LPAREN'
TT_RPAREN = 'RPAREN'
TT_EE = 'EE'
TT_NE = 'NE'
TT_LT = 'LT'
TT_GT = 'GT'
TT_LTE = 'LTE'
TT_GTE = 'GTE'
TT_EOF = 'EOF' 
TT_POW = 'POW'

KEYWORDS = [
     'wish', 'nuhuh', 'alsoo', 'or', 'if', 'then', 'elif', 'else'
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
            elif self.current_char == '^':
                tokens.append(token(TT_POW, pos_start=self.pos))
                self.advance()
            elif self.current_char in LETTERS:
                tokens.append(self.make_identifier())
            elif self.current_char in DIGITS:
                tokens.append(self.make_number())
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

####################
#runtime result
####################
class rtresult:
    def __init__(self):
        self.value = None
        self.error = None
    
    def register(self, res):
        if res.error: self.error = res.error
        return res.value
        
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
 
    def atom(self):
        res = parseresult()
        tok = self.current_tok
        if tok.type in (TT_INT, TT_FLOAT):
            res.register_advancement()
            self.advance()
            return res.success(number_node(tok))
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
        elif tok.matches(TT_KEYWORD, 'if'):
            if_expr = res.register(self.if_expr())
            if res.error: return res
            return res.success(if_expr)

        return res.failure(invalidsyntax(tok.pos_start, tok.pos_end, "Expected int or float or + or - or ( "))

    def power(self):
        return self.bin_op(self.atom, (TT_POW, ), self.factor)

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
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "Expected int , float , +,   - , (, 'nuhuh "))
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
            return res.failure(invalidsyntax(self.current_tok.pos_start, self.current_tok.pos_end, "expected var or int or float or identifier + - pr ( or )"))
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
class number:
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
            
    def subbed_by(self, other):
        if isinstance(other, number):
            return number(self.value-other.value).set_context(self.context), None
            
    def multed_by(self, other):
        if isinstance(other, number):
            return number(self.value*other.value).set_context(self.context), None
            
    def powed_by(self, other):
        if isinstance(other, number):
            return number(self.value**other.value).set_context(self.context), None
            
    def dived_by(self, other):
        if isinstance(other, number):
            if other.value == 0:
                return None, rterror(other.pos_start, other.pos_end,  'Div by 0 error omg thts so stupid', self.context )
            return number(self.value/other.value).set_context(self.context), None
    
    def get_comparision_eq(self, other):
        if isinstance(other, number):
            return number(int(self.value == other.value)).set_context(self.context), None
    
    def get_comparision_ne(self, other):
        if isinstance(other, number):
            return number(int(self.value != other.value)).set_context(self.context), None
    
    def get_comparision_lt(self, other):
        if isinstance(other, number):
            return number(int(self.value < other.value)).set_context(self.context), None
    
    def get_comparision_gt(self, other):
        if isinstance(other, number):
            return number(int(self.value > other.value)).set_context(self.context), None
    
    def get_comparision_lte(self, other):
        if isinstance(other, number):
            return number(int(self.value <= other.value)).set_context(self.context), None
    
    def get_comparision_gte(self, other):
        if isinstance(other, number):
            return number(int(self.value >= other.value)).set_context(self.context), None
    
    def anded_by(self, other):
        if isinstance(other, number):
            return number(int(self.value and other.value)).set_context(self.context), None
    
    def ored_by(self, other):
        if isinstance(other, number):
            return number(int(self.value or other.value)).set_context(self.context), None
    
    def notted(self):
        return number(1 if self.value==0 else 0).set_context(self.context), None

    def is_true(self):
        return self.value != 0  # Fixed typo: was 'vlaue'

    def __repr__(self):
        return str(self.value)

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
    def __init__(self):
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

#############
###RUN#######
#############
global_symbol_table = symbol_table()
global_symbol_table.set("null", number(0))
global_symbol_table.set("true", number(1))
global_symbol_table.set("false", number(0))

def run(fn, text):
    # 1) Tokenize
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

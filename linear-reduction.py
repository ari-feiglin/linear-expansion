import math

class ExtNum:
    def __init__(self, n, inf=0):
        self.n = n
        self.inf = inf

    def __eq__(self, other):
        return self.inf == other.inf and self.n == other.n

    def __lt__(self, other):
        if self.inf < other.inf: return True
        if self.inf > other.inf: return False
        return self.n < other.n

    def __gt__(self, other):
        return other < self

    def __le__(self, other):
        if self.inf < other.inf: return True
        if self.inf > other.inf: return False
        return self.n <= other.n

    def __ge__(self, other):
        return other <= self

    def __str__(self):
        if self.inf == 0:
            return str(self.n)
        return f"{self.inf}{{{self.n}}}"

class Atom:
    def __init__(self, val, priority, atype, val_fun, str_rep=None):
        self.val = val
        self.priority = priority
        self.atype = atype
        self.val_fun = val_fun
        if str_rep is None:
            self.str_rep = str(val)
        else:
            self.str_rep = str_rep
    def __str__(self):
        return f"{self.atype}[{self.val}:{self.priority}]"

priorities = {
            '+' : ExtNum(1),
            '*' : ExtNum(2),
            '/' : ExtNum(2),
            '**' : ExtNum(3),
            ';' : ExtNum(0),
            '=' : ExtNum(0),
            ',' : ExtNum(0),
        }

def create_variable(name, value):
    macros[name.val] = value

operations = {
            '+' : lambda a,b: a.val + b.val,
            '*' : lambda a,b: a.val * b.val,
            '/' : lambda a,b: a.val / b.val,
            '**' : lambda a,b: a.val ** b.val,
            '=' : create_variable,
        }

def print_val(a):
    print(a.val)
    return Atom(None, None, None, None, None);

macros = {
            'sqrt' : lambda a: Atom(math.sqrt(a.val), a.priority, 'v', a.val_fun, 'sqrt' + a.str_rep),
            'let' : lambda a: Atom(a.val, a.priority, 'u', a.val_fun, 'let' + a.str_rep),
            'print' : print_val,
            'pow' : lambda a: Atom(math.pow(a.val[0], a.val[1]), a.priority, 'v', a.val_fun, 'pow' + a.str_rep),
            'sum' : lambda a: Atom(sum(a.val), a.priority, 'v', a.val_fun, 'sum' + a.str_rep),
        }

applications = {
    ('v','v') : lambda a,b: Atom(a.val * b.val, b.priority, 'v', b.val_fun, a.str_rep + b.str_rep),
    ('v','o') : lambda a,b: Atom(a.val, b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('o','v') : lambda a,b: Atom(a.val_fun(a, b), b.priority, 'v', b.val_fun, a.str_rep + b.str_rep),
    ('o','o') : lambda a,b: Atom(a.val_fun(a, b), b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('(',')') : lambda a,b: Atom(b.val, a.priority, 'v', b.val_fun, a.str_rep + b.str_rep),
    ('v',')') : lambda a,b: Atom(a.val, b.priority, ')', a.val_fun, a.str_rep + b.str_rep),
    ('o',')') : lambda a,b: Atom(a.val_fun(a, b), a.priority, ')', b.val_fun, a.str_rep + b.str_rep),
    ('s','v') : lambda a,b: macros[a.val](b),
    ('s','o') : lambda a,b: Atom(macros[a.val].val, b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('s',')') : lambda a,b: Atom(macros[a.val].val, b.priority, ')', a.val_fun, a.str_rep + b.str_rep),
    ('s','s') : lambda a,b: macros[a.val](b),
    ('o','s') : lambda a,b: Atom(a.val_fun(a, macros[b.val]), b.priority, 'v', b.val_fun, a.str_rep + b.str_rep),
    ('u','o') : lambda a,b: Atom(a.val, b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('v',';') : lambda a,b: Atom(a.val, b.priority, 'v', a.val_fun, a.str_rep + b.str_rep),
    ('s',';') : lambda a,b: Atom(a.val, b.priority, 's', a.val_fun, a.str_rep + b.str_rep),
    (',',',') : lambda a,b: Atom(a.val + b.val, b.priority, ',', a.val_fun, a.str_rep + b.str_rep),
    ('v',',') : lambda a,b: Atom([a.val], b.priority, ',', a.val_fun, a.str_rep + b.str_rep),
    (',',')') : lambda a,b: Atom(a.val + [b.val], b.priority, ')', a.val_fun, a.str_rep + b.str_rep),
}

atom_types = {
            'v' : lambda i: Atom(float(i), ExtNum(0,1), 'v', lambda a,b: Exception("integer type cannot compute value")),
            '(' : lambda i: Atom('(', ExtNum(0,1), '(', lambda a,b: Exception("parentheses type cannot compute value")),
            ')' : lambda i: Atom(')', ExtNum(0,0), ')', lambda a,b: Exception("parentheses type cannot compute value")),
            'o' : lambda o: Atom(o, priorities[o], 'o', operations[o]),
            's' : lambda s: Atom(s, ExtNum(0,1), 's', lambda: Exception("string type cannot compute value")),
            ';' : lambda s: Atom(';', ExtNum(0), ';', lambda: Exception("end type cannot compute value")),
            ',' : lambda s: Atom(',', ExtNum(0), ',', lambda: Exception("list constructor type cannot compute value")),
        }

independent_atypes = ['(', ')']

atypes = {
            ' ' : None,
            ';' : [';'],
            '(' : ['('],
            ')' : [')'],
            '.' : ['o', 'v'],
            ',' : [',', 'o']
        }

for c in ['~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '/', '|', '<', '>']:
    atypes[c] = ['o']

for i in range(10):
    atypes[str(i)] = ['v', 's']

for c in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ":
    atypes[c] = ['s']

def add_atom(atoms, tok, atype):
    if atype == None: return
    atoms.append(atom_types[atype](tok))

def convert_to_atoms(s):
    curr_tok = ''
    curr_atype = None
    atoms = []

    for c in s:
        if c not in atypes:
            Exception(f"Character {c} invalid")
        atype = atypes[c]

        if atype == None:
            add_atom(atoms, curr_tok, curr_atype)
            curr_atype = None
            curr_tok = ''
        elif curr_atype == None:
            curr_tok = c
            curr_atype = atype[0]
        elif curr_atype in independent_atypes or curr_atype not in atype:
            add_atom(atoms, curr_tok, curr_atype)
            curr_atype = atype[0]
            curr_tok = c
        else:
            curr_tok += c
    add_atom(atoms, curr_tok, curr_atype)

    return atoms + []

def print_atoms_list(atoms):
    for atom in atoms:
        print(atom, end=" ")
    print("")

def reduce_atoms(atoms):
    if len(atoms) == 1: return None
    if atoms[0].priority >= atoms[1].priority and (atoms[0].atype, atoms[1].atype) in applications:
        new_atom = applications[(atoms[0].atype,atoms[1].atype)](atoms[0], atoms[1])
        return [new_atom] + atoms[2:] if new_atom.val != None else atoms[2:]
    else:
        return [atoms[0]] + reduce_atoms(atoms[1:])

def full_reduce(atoms):
    last_atoms = atoms
    while atoms != None:
        print_atoms_list(atoms)
        last_atoms = atoms
        atoms = reduce_atoms(atoms)
    return last_atoms[0].val

expression = input(">> ")

atoms = convert_to_atoms(expression)
full_reduce(atoms)


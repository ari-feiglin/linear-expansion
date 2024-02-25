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

def get_priority(x):
    if x.isdigit():
        return ExtNum(None)
    else:
        priorities = {'+' : 1, '*' : 2, '**' : 3, ';' : 0}
        return ExtNum(priorities[x])

operations = {
            '+' : lambda a,b: a + b,
            '*' : lambda a,b: a * b,
            '**' : lambda a,b: a ** b
        }

functions = {
            'sqrt' : lambda a: math.sqrt(a)
        }

applications = {
    ('i','o') : lambda a,b: Atom(a.val, b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('o','i') : lambda a,b: Atom(a.val_fun(a.val, b.val), a.priority, 'i', b.val_fun, a.str_rep + b.str_rep),
    ('o','o') : lambda a,b: Atom(a.val_fun(a.val, b.val), b.priority, 'o', b.val_fun, a.str_rep + b.str_rep),
    ('(',')') : lambda a,b: Atom(b.val, a.priority, 'i', b.val_fun, a.str_rep + b.str_rep),
    ('i',')') : lambda a,b: Atom(a.val, b.priority, ')', a.val_fun, a.str_rep + b.str_rep),
    ('o',')') : lambda a,b: Atom(a.val_fun(a.val, b.val), a.priority, ')', b.val_fun, a.str_rep + b.str_rep),
    ('f','i') : lambda a,b: Atom(a.val_fun(b.val), b.priority, 'i', b.val_fun, a.str_rep + b.str_rep)
}

atom_types = {
            'i' : lambda i: Atom(float(i), ExtNum(0,1), 'i', lambda a,b: Exception("integer type cannot compute value")),
            '(' : lambda i: Atom('(', ExtNum(0,1), '(', lambda a,b: Exception("parentheses type cannot compute value")),
            ')' : lambda i: Atom(')', ExtNum(0,0), ')', lambda a,b: Exception("parentheses type cannot compute value")),
            'o' : lambda o: Atom(o, get_priority(o), 'o', operations[o]),
            'f' : lambda f: Atom(f, ExtNum(0,1), 'f', functions[f])
        }

independent_atypes = ['(', ')']

atypes = {
            ' ' : None,
            ';' : None,
            '(' : ['('],
            ')' : [')'],
            '.' : ['o', 'i']
        }

for c in ['~', '!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '/', '|', '<', '>']:
    atypes[c] = ['o']

for i in range(10):
    atypes[str(i)] = ['i', 'f']

for c in "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ":
    atypes[c] = ['f']

def add_atom(atoms, tok, atype):
    if atype == None: return
    atoms.append(atom_types[atype](tok))

def convert_to_atoms(s):
    curr_tok = ''
    curr_atype = None
    atoms = [atom_types['('](None)]

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

    return atoms + [atom_types[')'](None)]

def print_atoms_list(atoms):
    for atom in atoms:
        print(atom, end=" ")
    print("")

def reduce_atoms(atoms):
    if len(atoms) == 1: return None
    if atoms[0].priority >= atoms[1].priority and (atoms[0].atype, atoms[1].atype) in applications:
        return [applications[(atoms[0].atype,atoms[1].atype)](atoms[0], atoms[1])] + atoms[2:]
    else:
        return [atoms[0]] + reduce_atoms(atoms[1:])

def full_reduce(atoms):
    last_atoms = atoms
    while atoms != None:
        print_atoms_list(atoms)
        last_atoms = atoms
        atoms = reduce_atoms(atoms)
    return last_atoms[0].val

test = "sqrt((2+3)) + 3.2;"

print(test)
atoms = convert_to_atoms(test)
print_atoms_list(atoms)
print(full_reduce(atoms))


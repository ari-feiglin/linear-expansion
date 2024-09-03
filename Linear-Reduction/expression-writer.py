from random import randint, choice

def generate_string(depth, number):
    string = ""
    for i in range(number):
        if depth > 0 and randint(0,5) == 0:
            string += "(" + generate_string(depth - 1, number) + ")"
        else:
            string += str(randint(0,100))
        string += choice(("+", "-", "*")) + " "
    string += str(randint(-100,100))
    return string

with open("expression.txt", "w") as f:
    s = generate_string(10,5)
    print(f"Value: {eval(s)}")
    f.write("_prim_print (" + s + ");")


import random, string

NUM_TESTS = 100000
MAX_DEPTH = 10
TEST_FILE = './test.txt'

def generateExpression (n) :
    term = random.choice(['Abs', 'App', 'Var']) if n > 1 else 'Var'
    match term :
        case 'Abs' :
            var = random.choice(string.ascii_lowercase)
            return f'(λ{var}.{generateExpression(n-1)})'
        case 'App' :
            return f'({generateExpression(n-1)} {generateExpression(n-1)})'
        case 'Var' :
            var = random.choice(string.ascii_lowercase)
            return f'{var}'

def main () :
    testExpressions = [ generateExpression(MAX_DEPTH) for _ in range(NUM_TESTS) ]
    with open(TEST_FILE, 'w') as f:
        f.writelines('\n'.join(testExpressions))

if __name__ == '__main__' :
    random.seed(123)
    main()
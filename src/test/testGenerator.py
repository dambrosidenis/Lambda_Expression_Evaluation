import random, string, sys

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
    """
    Usage: python3 testGenerator <outputFile> <examplesGenerated> <maxDepth>

    Note that if an argument is not provided, the default values will be used:
        - outputFile:           './test.txt/'
        - examplesGenerated:    10
        - maxDepth:             10
    """
    TEST_FILE = './test.txt' if len(sys.argv) < 2 else sys.argv[1]
    NUM_TESTS = 10 if len(sys.argv) < 3 else int(sys.argv[2])
    MAX_DEPTH = 5 if len(sys.argv) < 4 else int(sys.argv[3])
    testExpressions = [ generateExpression(MAX_DEPTH) for _ in range(NUM_TESTS) ]
    with open(TEST_FILE, 'w') as f:
        f.writelines('\n'.join(testExpressions))

if __name__ == '__main__' :
    main()


def parse(filename):
    puzzle = {}

    with open(filename, 'r') as f:
        y = 0
        for line in f.readlines():
            if line[0] == '-':
                continue
            line = line.strip()
            x = 0
            for char in line:
                if char == '|':
                    continue
                if char == '.':
                    puzzle[(x,y)] = None
                else:
                    puzzle[(x,y)] = int(char)
                x+=1
            y+=1

    return puzzle

def tostring(puzzle):
    ret=''
    for y in xrange(0,9):
        for x in xrange(0, 9):
            digit = puzzle[(x, y)]
            if digit == None:
                ret+='.'
            else:
                ret+=str(digit)
        ret+='\n'
    return ret

def grid_of(square):
    (row, column) = square
    return (row/3, column/3)

def find_choices(puzzle):
    choices = {}

    for x in xrange(0, 9):
        for y in xrange(0, 9):
            if puzzle[(x,y)]: # that square is already filled
                continue

            choices[(x,y)] = remaining_in_row(puzzle, y) & remaining_in_column(puzzle, x) & remaining_in_grid(puzzle, grid_of((x,y)))
    
    return choices

def order_choices(choices):
    def num_possibilities(square):
        return len(choices[square])

    squares = choices.keys()
    squares.sort(key=num_possibilities)

    return squares

def solve(puzzle, past):
    # find_choices returns a dict that for every unfilled square, gives you the possible digits.
    # if the result is an empty dict, then there are no more unfilled squares, so you win! The algorithm only ever makes valid choices, assuming the puzzle isn't wrong
    choices = find_choices(puzzle)
    if len(choices) == 0:
        return puzzle

    # This means there is a logical contradiction based on an earlier choice, so backtrack
    ordered_squares = order_choices(choices)
    if len(choices[ordered_squares[0]]) == 0:
        return None

    for square in ordered_squares:
        possible_digits = choices[square]

        if len(possible_digits) == 1:
            display_char='*'
        else:
            display_char='?'
        newpast = past+display_char

        for digit in possible_digits:
            modified_puzzle = dict(puzzle)
            modified_puzzle[square] = digit
            #print tostring(modified_puzzle)
            ##print '*'*(81-len(choices))
            print newpast
            possible_solution = solve(modified_puzzle, newpast)
            if possible_solution:
                return possible_solution

    # there is no solution given the previous choices
    return None

def remaining_in_column(puzzle, x):
    numbers_in_column = set()
    for y in xrange(0, 9):
        digit = puzzle[(x, y)]
        if digit != None:
            numbers_in_column.add(digit)
    return remaining_in_set(numbers_in_column)

def remaining_in_row(puzzle, y):
    numbers_in_row = set()
    for x in xrange(0, 9):
        digit = puzzle[(x, y)]
        if digit != None:
            numbers_in_row.add(digit)
    return remaining_in_set(numbers_in_row)

def remaining_in_grid(puzzle, grid_id):
    (x, y) = grid_id
    left_row = x*3
    right_row = left_row+3
    
    left_column = y*3
    right_column = left_column+3

    numbers_in_grid = set()
    for row in xrange(left_row, right_row):
        for column in xrange(left_column, right_column):
            digit = puzzle[(row, column)]
            if digit != None:
                numbers_in_grid.add(digit)
    return remaining_in_set(numbers_in_grid)

def remaining_in_set(s):
    return set(xrange(1,10)) - s



if __name__ == '__main__':
    import sys
    print tostring(solve(parse(sys.argv[1]), ""))


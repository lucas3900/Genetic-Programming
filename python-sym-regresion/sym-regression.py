"""
Lucas Barusek 
Project to implement symbolic regression using genetic programming.
Hard coded to work for the simple function, and 3 datasets from
https://github.com/EpistasisLab/pmlb/tree/master/datasets
    -   192_vineyard.tsv
    -   195_auto_price.tsv
    -   1096_FacultySalaries.tsv

Running the Program:
    1. python3 project3.py
    2. Follow the input statements
"""

import operator, random, math, copy, csv
from numpy import random as npRandom
from statistics import pstdev
import matplotlib.pyplot as plt

POPULATION_SIZE = 750
MAX_GENERATIONS = 50
MAX_FLOAT = 1e12

def safe_division(numerator, denominator):
    """Divides numerator by denominator. If denominator is close to 0, returns
    MAX_FLOAT as an approximate of infinity."""
    if abs(denominator) <= 1 / MAX_FLOAT:
        return MAX_FLOAT
    return numerator / denominator

def safe_log(argument):
    if argument <= 0:
        return MAX_FLOAT
    return math.log(argument)

def safe_exp(power):
    """Takes e^power. If this results in a math overflow, or is greater
    than MAX_FLOAT, instead returns MAX_FLOAT"""
    try:
        result = math.exp(power)
        if result > MAX_FLOAT:
            return MAX_FLOAT
        return result
    except OverflowError:
        return MAX_FLOAT

# Dictionary mapping function stings to functions that perform them
FUNCTION_DICT = {"+": operator.add,
                 "-": operator.sub,
                 "*": operator.mul,
                 "/": safe_division,
                 "exp": safe_exp,
                 "sin": math.sin,
                 "cos": math.cos,
                 "ln": safe_log}

# Dictionary mapping function strings to their arities (number of arguments)
FUNCTION_ARITIES = {"+": 2,
                    "-": 2,
                    "*": 2,
                    "/": 2,
                    "exp": 1,
                    "sin": 1,
                    "cos": 1,
                    "ln": 1}

# List of function strings
FUNCTIONS = list(FUNCTION_DICT.keys())

# Strings for each variable
# can only handle up to 20 variable datasets
VARIABLES = [f"x{num}" for num in range(20)]



class TerminalNode:
    """Leaf nodes that contain terminals."""

    def __init__(self, value):
        """value might be a literal (i.e. 5.32), or a variable as a string."""
        self.value = value

    def __str__(self):
        return str(self.value)

    def eval(self, variable_assignments):
        """Evaluates node given a dictionary of variable assignments."""

        if self.value in VARIABLES:
            return variable_assignments[self.value]

        return self.value

    def tree_depth(self):
        """Returns the total depth of tree passrooted at this node.
        Since this is a terminal node, this is just 0."""

        return 0

    def size_of_subtree(self):
        """Gives the size of the subtree of this node, in number of nodes.
        Since this is a terminal node, this is just 1."""

        return 1

class FunctionNode:
    """Internal nodes that contain functions."""

    def __init__(self, function_symbol, children):
        self.function_symbol = function_symbol
        self.function = FUNCTION_DICT[self.function_symbol]
        self.children = children

        assert len(self.children) == FUNCTION_ARITIES[self.function_symbol]

    def __str__(self):
        """This should make printed programs look like Lisp."""

        result = f"({self.function_symbol}"
        for child in self.children:
            result += " " + str(child)
        result += ")"
        return result

    def eval(self, variable_assignments):
        """Evaluates node given a dictionary of variable assignments."""

        # Calculate the values of children nodes
        children_results = [child.eval(variable_assignments) for child in self.children]

        # Apply function to children_results.
        return self.function(*children_results)

    def tree_depth(self):
        """Returns the total depth of tree rooted at this node."""

        return 1 + max(child.tree_depth() for child in self.children)

    def size_of_subtree(self):
        """Gives the size of the subtree of this node, in number of nodes."""

        return 1 + sum(child.size_of_subtree() for child in self.children)


################################################################################
################################################################################
################################################################################


class Individual:
    """ Represents an Individual Function """

    def __init__(self, program):
        self._program = program
        self._errorVector = []
        self._totalError = 0

    def evaluateTestCases(self, testCases):   
        """ Evaluates each test case and adds the error for that test 
        case to the error vector, and then sums the total error """
        for dataset in testCases:
            self._errorVector.append(self.evaluateSingleCase(dataset))
        
        self._totalError = sum(self._errorVector)

    def evaluateSingleCase(self, case):
        """ Evaluate a single test case """
        value = self._program.eval(case)
        return abs(case['y'] - value)

    def getTotalError(self):
        return self._totalError

    def getProgramSize(self):
        return self._program.tree_depth()

    def getNumNodes(self):
        return self._program.size_of_subtree()

    def getProgram(self):
        return self._program


def model(x0, x1):
    """ Simple model from project sheet """
    return x0**2 + math.log(math.sin(x1) + 2)


def generateTestCases(dataset):
    """ Generates the test cases """
    testCases = []
    # random test cases for simple model
    if dataset == 'random':
        while len(testCases) < 35:
            x0 = random.uniform(-3, 3)
            x1 = random.uniform(-5, 5)
            row_dict = {'y': model(x0,x1)}
            row_dict['x0'] = x0
            row_dict['x1'] = x1
            testCases.append(row_dict)
        return (testCases, 2)
    # static test cases for simple model
    elif dataset == 'static':
        # 7 x0 values
        for x0 in range(-3, 4, 1):
            # 5 x1 values
            for x1 in range(-5, 5, 2):
                row_dict = {'y': model(x0,x1)}
                row_dict['x0'] = x0
                row_dict['x1'] = x1
                testCases.append(row_dict)
    # training cases for symbolic regression problem from plmb
    else:
        return make_csv_training_cases(dataset)
        
    return (testCases, 2)


def pointMutation(parent1, Vars):
    """ Implementation of point mutation. Replaces one node with another
    of the same arity """
    tree1 = parent1.getProgram()
    randomNode = random_subtree(tree1)
    if randomNode.tree_depth() == 0: # terminal node
        if random.random() < 0.5:
            terminal_value = random.choice(VARIABLES[:Vars])
        else:
            terminal_value = random.uniform(-10.0, 10.0)
        randomNode.value = terminal_value
    else: # Function Node
        numChildren = len(randomNode.children)
        arity = -1
        while numChildren != arity:
            function_symbol = random.choice(FUNCTIONS)
            arity = FUNCTION_ARITIES[function_symbol]
        randomNode.function_symbol = function_symbol
        randomNode.function = FUNCTION_DICT[function_symbol]
    
    return parent1


def addDeltaToConstants(node, delta):
    """ adds a delta value to all constants in the tree """
    if node.tree_depth() == 0:
        if node.value not in VARIABLES:
            node.value += delta
    else:
        for child in node.children:
            addDeltaToConstants(child, delta)


def constantMutation(parent1):
    """ Implementation of constant mutation. Applies a random gaussian value
    to every constant in the tree """
    tree1 = parent1.getProgram()
    # normal distribution with 2 standard deviations
    delta = npRandom.normal(0, 2, 1)[0]
    addDeltaToConstants(tree1, delta)
    return parent1


def hoistMutation(parent1):
    """ returns a subtree of the parent """
    tree1 = parent1.getProgram()
    return Individual(random_subtree(tree1))


def sizeFairMutation(parent1, Vars):
    """ Mutates the tree such that the new random tree is about the same
    size as the one it is replacing """
    tree1 = parent1.getProgram()
    nodes = tree1.size_of_subtree()
    indexOfOldSubtree = random.randint(0, nodes - 1)
    oldDepth = subtree_at_index(tree1, indexOfOldSubtree).tree_depth()
    newDepth = random.randint(round(oldDepth / 2), round((3 * oldDepth) / 2))
    newSubtree = generate_tree_grow(newDepth, Vars)
    replace_subtree_at_index(tree1, indexOfOldSubtree, newSubtree)
    return parent1


def subtreeMutation(parent1, Vars):
    """ Replace one subtree in the parent with a random subtree """
    tree1 = parent1.getProgram()
    randomSubtree = generate_tree_grow(2, Vars) # short random subtree
    newProgram = replace_random_subtree(tree1, randomSubtree)
    return Individual(newProgram)


def sizeFairCrossover(parent1, parent2):
    """ Take one subtree from one of the parents, and replace it with 
    a subtree of about the same size in the other parent """
    # ensure smaller tree will end up getting the new code
    if parent1.getProgramSize() < parent2.getProgramSize():
        tree1 = parent1.getProgram()
        tree2 = parent2.getProgram()
    else: 
        tree1 = parent2.getProgram()
        tree2 = parent1.getProgram()
    nodes = tree1.size_of_subtree()
    indexInParent1 = random.randint(0, nodes - 1)
    depthOfReplace = subtree_at_index(tree1, indexInParent1).tree_depth()
    newDepth = MAX_FLOAT
    while newDepth not in range(round(depthOfReplace / 2), round((3 * depthOfReplace) / 2) + 1):
        newSubtree = random_subtree(tree2)
        newDepth = newSubtree.tree_depth()
    newProgram = replace_subtree_at_index(tree1, indexInParent1, newSubtree)
    return Individual(newProgram)


def crossover(parent1, parent2):
    """ Replace one random subtree in parent 2 with one random subtree from parent 1 """
    tree1 = parent1.getProgram()
    tree2 = parent2.getProgram()
    newProgram = replace_random_subtree(tree2, random_subtree(tree1))
    return Individual(newProgram)


def epsilonLexicase(candidates, testCases):
    """ Implementation of epsilon lexicase, where epsilon = 10 """
    shuffledCases = copy.deepcopy(testCases)
    random.shuffle(shuffledCases)
    eligible = copy.deepcopy(candidates)
    for case in shuffledCases:
        survivors = []
        errors = [individual.evaluateSingleCase(case) for individual in eligible]
        bestError = min(errors)
        for individual in range(len(eligible)):
            if errors[individual] <= bestError + 10:
                survivors.append(eligible[individual])
        if len(survivors) == 1:
            return survivors[0]
        eligible = survivors
            
    # we've exhausted every case, and there are still some cases
    return random.choice(eligible)


def tournamentSelection(candidates):
    """ Randomly select 6 parents and return the individual with the smallest total error """
    participants = [random.choice(candidates) for _ in range(7)]
    totalErrors = [participant.getTotalError() for participant in participants]
    return participants[totalErrors.index(min(totalErrors))]


def getParetoFront(coords):
    #[0] is going to be the error and [1] is going to be the program size
    paretoFront = [coords[0]]    
    for pair in coords[1:]:
        if pair[1] <= paretoFront[-1][1]:
            paretoFront.append(pair)

    return paretoFront


def paretoTournamentSelection(candidates):
    participants = [random.choice(candidates) for _ in range(7)]
    paretoCoord = [(p.getTotalError(), p.getProgramSize()) for p in participants]
    paretoFront = getParetoFront(sorted(paretoCoord, key = lambda x: x[0]))
    coords = paretoFront[random.randint(len(paretoFront) // 6, len(paretoFront) // 5)]
    return participants[paretoCoord.index(coords)]
    

def variation(currentGen, pareto, lexicase, bloatControl, Vars, testCases):
    """ Implements making the next generation 
    70% of time we will do crossover
    20% of time we will do mutation
    10% of time we will do reproduction
    """
    typeOfVariation = random.random()
    if pareto:
        parent1 = paretoTournamentSelection(currentGen)
    elif lexicase:
        parent1 = epsilonLexicase(currentGen, testCases)
    else:
        parent1 = tournamentSelection(currentGen)
    if typeOfVariation < .7: # crossover
        if pareto:
            parent2 = paretoTournamentSelection(currentGen)
        elif lexicase:
            parent2 = epsilonLexicase(currentGen, testCases)
        else:
            parent2 = tournamentSelection(currentGen)
        return crossover(parent1, parent2)
    elif typeOfVariation < 0.9: # mutation
        if bloatControl:
            typeOfMutation = random.random()
            if typeOfMutation < 0.6:
                return sizeFairMutation(parent1, Vars)
            elif typeOfMutation < 0.8:
                return constantMutation(parent1)
            else:
                return hoistMutation(parent1)
        else:
            return subtreeMutation(parent1, Vars)
    else: # reproduction
        return parent1


def getAverage(lst):
    """ Simple average calculator rounded to two decimal points """
    return round(sum(lst) / len(lst), 2)


def sumStats(generation, individuals, errorTrackers, sizeTrackers):
    """ Prints generational stats, and saves minimum error and average size """
    print(f"GENERATION {generation}")
    errors = [individual.getTotalError() for individual in individuals]
    depths = [individual.getProgramSize() for individual in individuals]
    numNodes = [Individual.getNumNodes() for Individual in individuals]
    errorTrackers.append(round(min(errors), 2)); sizeTrackers.append(getAverage(numNodes))
    print(f"ERROR - Average: {getAverage(errors)}, Min: {round(min(errors), 2)}, Max: {round(max(errors), 2)}, Std: {round(pstdev(errors), 2)}")
    print(f"DEPTH - Average: {getAverage(depths)}, Min: {round(min(depths), 2)}, Max: {round(max(depths), 2)}, Std: {round(pstdev(depths), 2)}")
    print(f"NODES - Average: {getAverage(numNodes)}, Min: {round(min(numNodes), 2)}, Max: {round(max(numNodes), 2)}, Std: {round(pstdev(numNodes), 2)}")
    print()


def generationalCycle(firstGen, testCases, pareto, lexicase, uniform, 
                      bloatControl, Vars, errorTracker, sizeTracker):
    """ Implementation of the generational cycle for genetic programming
        1. evaluate all the individuals
        2. Print summary stats for the current generation
        3. create new generation from current generation
     """
    currentGen = firstGen
    for i in range(MAX_GENERATIONS - 1):
        for individual in currentGen:
            individual.evaluateTestCases(testCases)
        sumStats(i+1, currentGen, errorTracker, sizeTracker)
        if uniform:
            newGen = []
            buckets = [25]*30
            while buckets != [0]*30:
                child = variation(currentGen, pareto, lexicase, bloatControl, Vars, testCases)
                size = child.getNumNodes()
                if size <= 30 and buckets[size-1] > 0:
                    buckets[size-1] -= 1
                    newGen.append(child)
            currentGen = newGen
        else:
            currentGen = [variation(currentGen, pareto, lexicase, bloatControl, Vars, testCases) for _ in range(POPULATION_SIZE)]
        
    # return the last generation of programs
    return currentGen


def programSelection(finalGen, testCases):
    """ Picks the final program based on which one has the smallest error """
    for individual in finalGen:
        individual.evaluateTestCases(testCases)
    totalErrors = [individual.getTotalError() for individual in finalGen]
    return finalGen[totalErrors.index(min(totalErrors))]


def symbolicRegressionGP(pareto=False, lexicase=False, uniform=False, bloatControl=False, dataset='static'):
    """ Main driver of the genetic progamming algorithm """
    # initialization of random programs
    errors = []
    sizes = []
    testCases, variables = generateTestCases(dataset)
    firstGeneration = [Individual(generate_random_program(variables)) for _ in range(POPULATION_SIZE)]
    sumStats(0, firstGeneration, errors, sizes)
    finalGen = generationalCycle(firstGeneration, testCases, pareto, lexicase, uniform, bloatControl, variables, errors, sizes)
    finalProgram = programSelection(finalGen, testCases)
    print(f"FINAL PROGRAM: {finalProgram.getProgram()}")
    print(f"Error: {finalProgram.getTotalError()}, Depth: {finalProgram.getProgramSize()}, Nodes: {finalProgram.getNumNodes()}")
    return (finalProgram, errors, sizes)


###############################################################################
###############################################################################
###############################################################################


def random_terminal(Vars):
    """Returns a random TerminalNode
    Half the time pick a random variable, and half the time a random float in
    the range [-10.0, 10.0]"""

    if random.random() < 0.5:
        terminal_value = random.choice(VARIABLES[:Vars])
    else:
        terminal_value = random.uniform(-10.0, 10.0)

    return TerminalNode(terminal_value)


def generate_tree_full(max_depth, Vars):
    """Generates and returns a new tree using the Full method for tree
    generation and a given max_depth."""

    if max_depth <= 0:
        return random_terminal(Vars)

    function_symbol = random.choice(FUNCTIONS)
    arity = FUNCTION_ARITIES[function_symbol]
    children = [generate_tree_full(max_depth - 1, Vars) for _ in range(arity)]

    return FunctionNode(function_symbol, children)


def generate_tree_grow(max_depth, Vars):
    """Generates and returns a new tree using the Grow method for tree
    generation and a given max_depth."""

    ## What percent of the time do we want to select a terminal?
    percent_terminal = 0.25

    if max_depth <= 0 or random.random() < percent_terminal:
        return random_terminal(Vars)

    function_symbol = random.choice(FUNCTIONS)
    arity = FUNCTION_ARITIES[function_symbol]
    children = [generate_tree_grow(max_depth - 1, Vars) for _ in range(arity)]

    return FunctionNode(function_symbol, children)


def generate_random_program(Vars):
    """Creates a random program as a syntax tree.
    This uses Ramped Half-and-Half.
    max-depth taken from the range [2, 5] inclusive."""

    depth = random.randint(2, 5)
    if random.random() < 0.5:
        return generate_tree_full(depth, Vars)
    else:
        return generate_tree_grow(depth, Vars)


def subtree_at_index(node, index):
    """Returns subtree at particular index in this tree. Traverses tree in
    depth-first order."""
    if index <= 0:
        return node
    # Subtract 1 for the current node
    index -= 1
    # Go through each child of the node, and find the one that contains this index
    for child in node.children:
        child_size = child.size_of_subtree()
        if index < child_size:
            return subtree_at_index(child, index)
        index -= child_size
    return "INDEX {} OUT OF BOUNDS".format(index)


def replace_subtree_at_index(node, index, new_subtree):
    """Replaces subtree at particular index in this tree. Traverses tree in
    depth-first order."""
    # Return the subtree if we've found index == 0
    if index <= 0:
        return new_subtree
    # Subtract 1 for the current node
    index -= 1
    # Go through each child of the node, and find the one that contains this index
    for child_index in range(len(node.children)):
        child_size = node.children[child_index].size_of_subtree()
        if index < child_size:
            new_child = replace_subtree_at_index(node.children[child_index], index, new_subtree)
            node.children[child_index] = new_child
            return node
        index -= child_size
    return "INDEX {} OUT OF BOUNDS".format(index)


def random_subtree(program):
    """Returns a random subtree from given program, selected uniformly."""
    nodes = program.size_of_subtree()
    node_index = random.randint(math.ceil((nodes - 1) / 3), nodes - 1)
    return subtree_at_index(program, node_index)


def replace_random_subtree(program, new_subtree):
    """Replaces a random subtree with new_subtree in program, with node to
    be replaced selected uniformly."""
    nodes = program.size_of_subtree()
    node_index = random.randint(0, nodes - 1)
    new_program = copy.deepcopy(program)
    return replace_subtree_at_index(new_program, node_index, new_subtree)


def read_data(filename, delimiter=",", has_header=True):
    """Reads classification data from a file.
    Returns a list of the header labels and a list containing each datapoint."""
    data = []
    header = []
    with open(filename) as f:
        reader = csv.reader(f, delimiter=delimiter)
        if has_header:
            header = next(reader, None)
        for line in reader:
            example = [float(x) for x in line]
            data.append(example)

        return header, data


def make_csv_training_cases(filename='vineyard'):
    """Makes a list of training cases. Each training case is a dictionary
    mapping x_i and y to their values. x_i starts at x0.
    Ex: {'x0': 1.0, 'x1': 5.0, 'y': 9.5}
    Right now, this is hard-coded to solve this problem:
    https://github.com/EpistasisLab/pmlb/tree/master/datasets/192_vineyard
    but it should be easy to adopt to other CSVs.
    """

    fileNames = {'vineyard':"192_vineyard.tsv", "salaries": "1096_FacultySalaries.tsv", "auto":"195_auto_price.tsv"}
    assert(filename in fileNames)
    _, data = read_data(fileNames[filename], "\t")
    cases = []

    for row in data:
        output = row[-1]
        inputs = row[:-1]
        row_dict = {"y": output}
        for i, input in enumerate(inputs):
            row_dict["x" + str(i)] = input
        cases.append(row_dict)

    return (cases, i + 1)


def getInput():
    """ Gets user input for GP """
    print("\n \
1. Simple Model: x0^2 + ln(sin(x1) + 2)\n \
2. 192_vineyard.tsv \n \
3. 195_auto_price.tsv\n \
4. 1096_FacultySalaries.tsv\n")
    model = input("Which model do you want to regress (1, 2, 3, 4)? ")
    assert(int(model) in [1, 2, 3, 4])
    if int(model) == 1:
        cases = input("STATIC or RANDOM Test Cases (S/r)? ")
        if cases.upper() in ["S", ""]:
            dataset = "static"
        else:
            dataset = "random"
    elif int(model) == 2: dataset = "vineyard"
    elif int(model) == 3: dataset = "auto"
    else: dataset = "salaries"

    userInput = input("Do you want to use the pareto front (y/N)? ")
    if userInput.upper() in ["N", ""]:
        pareto = False
    else:
        pareto = True 

    lexicase = False
    if not pareto:
        userInput = input("Do you want to use lexicase selection (y/N)? ")
        if userInput.upper() not in ["N", ""]:
            lexicase = True 

    userInput = input("Do you want to use the uniform distribution (y/N)? ")
    if userInput.upper() in ["N", ""]:
        uniform = False
    else:
        uniform = True 

    userInput = input("Do you want to use the bloat control (y/N)? ")
    if userInput.upper() in ["N", ""]:
        bloatControl = False
    else:
        bloatControl = True 

    return symbolicRegressionGP(pareto=pareto, lexicase=lexicase, uniform=uniform, bloatControl=bloatControl, dataset=dataset)


def main():
    program, errors, sizes = getInput()
    print("Program: ", program)
    print("Error: ", errors)
    print("Sizes: ", sizes)


if __name__ == "__main__":
    main()

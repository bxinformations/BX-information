"""Let us encode the N-queens problem with a simple FD constraint solver."""
import string

from constraint import Constraint, Model, State, Var, X_different_from_C, X_equal_C

### Question 1.
### Write the explanation of what you see after the first call to
### propagate_once_each() here

### after the first call to propogate_once_each(), since now all the constraints 
### are both consitant with each other, so each block are still valiad in this case
### that's why we got all black in the end.

### Question 2.
### Which Domain Filtering Principle does the call to is_entailed implement?

### Subsumption

### Question 3.
### Write the explanation of what you see after we add a constraint here

### after add a constraint, some block are not 

### Question 4.
### Write the explanation of what you see after the last call to
### propagate_once_each() here




def n_queens_model(number: int) -> Model:
    """N variables using the letters of the alphabet.

    (only works up to 26, obviously)
    """
    variables = list(string.ascii_uppercase[:number])
    # we add the variables and put the initial domains 1..N in a single step
    return Model(1, number, variables)


def add_n_queens_constraints(model: Model):
    """Add constraints for the usual N-queens problem."""
    variables = model.state.keys()
    for i, x in enumerate(variables):
        for j, y in enumerate(variables):
            if i < j:
                model.add_constraint(X_different_from_Y_plus_C(x, y, 0))
                model.add_constraint(X_different_from_Y_plus_C(x, y, j - i))
                model.add_constraint(X_different_from_Y_plus_C(x, y, i - j))


class X_different_from_Y_plus_C(Constraint):
    """Simple propagator for the constraint x != y + c.

    it only propagates something when one of the variables is instantiated
    """

    def __init__(self, x: Var, y: Var, c: int):
        self.x = x
        self.y = y
        self.c = c

    def __str__(self):
        return f'{self.x} != {self.y} + {self.c}'

    def apply(self, state: State) -> bool:
        has_changed = False
        if state.is_instantiated(self.x):
            val = state.get_value(self.x) - self.c
            has_changed |= val in state[self.y]
            state[self.y].remove_value(val)
        if state.is_instantiated(self.y):
            val = state.get_value(self.y) + self.c
            has_changed |= val in state[self.x]
            state[self.x].remove_value(val)
        return has_changed


class X_greaterthan_Y_plus_C(Constraint):
    def __init__(self, x: Var, y: Var, c: int):
        self.x = x
        self.y = y
        self.c = c

    def __str__(self):
        return f'{self.x} > {self.y} + {self.c}'

    def apply(self, state: State) -> bool:
        has_changed = False
        k = state[self.x].current_min
        l = state[self.x].current_max
        m = state[self.y].current_min
        n = state[self.y].current_max
        k1 = m + self.c
        n1 = l - self.c 
        has_changed |= (k1 <= k)
        has_changed |= (n1 >= n)
        if k1 > k:
            state[self.x].current_min = k1 
        if n1 < n:
            state[self.y].current_max = n1
        return has_changed


def repeated_inconsistent_propagation():
    model1 = Model(1, 10, ["x", "y"])
    model1.add_constraint(X_greaterthan_Y_plus_C("x","y",0))
    model1.add_constraint(X_greaterthan_Y_plus_C("y","x",0))
    
    model2 = Model(1, 10, ["x"])
    model2.add_constraint(X_greaterthan_Y_plus_C("X","X",0))


class X_equal_Y_plus_C(Constraint):
    def __init__(self, x: Var, y: Var, c: int):
        self.x = x
        self.y = y
        self.c = c

    def __str__(self):
        return f'{self.x} = {self.y} + {self.c}'

    def apply(self, state: State) -> bool:
        has_changed = False
        k = state[self.x].current_min
        l = state[self.x].current_max
        m = state[self.y].current_min
        n = state[self.y].current_max
        k1 = m + self.c
        l1 = n + self.c
        m1 = k - self.c 
        n1 = l - self.c
        if k1 > k:
            has_changed = True
            state[self.x].current_min = k1 
        if l1 < l:
            has_changed = True
            state[self.x].current_max = l1 
        if m1 > m:
            has_changed = True
            state[self.y].current_min = m1
        if n1 < n:
            has_changed = True
            state[self.y].current_max = n1
        return has_changed


def difference_between_equality_and_double_gt():
    model1 = Model(1, 10, ["x", "y"])
    model1.add_constraint(X_greaterthan_Y_plus_C("x","y",-1))
    model1.add_constraint(X_greaterthan_Y_plus_C("y","x",-1))
    
    model2 = Model(1, 10, ["x", "y"])
    model2.add_constraint(X_equal_Y_plus_C("x", "y", 0))


class X_times_Y_equal_Z(Constraint):
    def __init__(self, x: Var, y: Var, z: int):
        self.x = x
        self.y = y
        self.z = z

    def __str__(self):
        return f'{self.x} * {self.y} = {self.z}'

    def apply(self, state: State) -> bool:

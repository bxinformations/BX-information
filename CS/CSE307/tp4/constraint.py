"""Very simple implementation of a Finite-Domain constraint solver in Python."""
import copy
from typing import Dict, List, Optional

from graphviz import Digraph

import matplotlib.pyplot as plt


Var = str


class Domain:
    """A Domain has an internal representation, here a list of bools."""

    def __init__(self, dmin=0, dmax=128):
        """Set stuff up."""
        self.dmin = dmin
        self.dmax = dmax
        # Since True and False are singletons in Python, this is memory
        # inefficient, but not awful: it takes as many pointers as entries
        # A much better structure would be, for instance
        # https://pypi.org/project/pyroaring/
        self.domain = [True] * (dmax - dmin + 1)

    def __len__(self) -> int:
        """Make len(domain) work."""
        return self.dmax - self.dmin + 1

    def __str__(self) -> str:
        """Represent the domain as a string."""
        return str(map(lambda v: "#" if v else ".", self.domain))

    def __contains__(self, item):
        """Allow testing with "value in domain."""
        return self.dmin <= item <= self.dmax and self.domain[item - self.dmin]

    def as_bool(self) -> List[bool]:
        """Represent the domain as a list of booleans."""
        return self.domain

    def remove_value(self, value: int):
        """Remove a single value from the domain, i.e. make it False."""
        if self.dmin <= value <= self.dmax:
            self.domain[value - self.dmin] = False

    @property
    def current_min(self) -> Optional[int]:
        """Find the first True value in the domain."""
        try:
            return self.domain.index(True) + self.dmin
        except ValueError:
            return None

    @current_min.setter
    def current_min(self, dmin: int):
        """Make all values less than dmin False."""
        if dmin > self.dmin:
            self.domain[: dmin - self.dmin] = [False] * (dmin - self.dmin)

    @property
    def current_max(self) -> Optional[int]:
        """Find the last True value in the domain."""
        index = self.dmax - self.dmin
        while index >= 0 and not self.domain[index]:
            index -= 1
        if index >= 0:
            return index + self.dmin
        return None

    @current_max.setter
    def current_max(self, dmax: int):
        """Make all values greater than dmax False."""
        if dmax < self.dmax:
            self.domain[dmax - self.dmax:] = [False] * (self.dmax - dmax)

    def is_instantiated(self) -> bool:
        """Very inefficient check for single True value."""
        return self.domain.count(True) == 1

    def get_value(self) -> Optional[int]:
        """Very inefficient way to find the single True value."""
        if self.is_instantiated():
            return self.current_min
        # else:
        return None

    def is_failed(self) -> bool:
        """Return a boolean for domain emptyness."""
        return self.domain.count(True) == 0


class State(Dict[Var, Domain]):
    """A State is a mapping from Vars to Domains, with a few utility methods."""

    def is_instantiated(self, var: Var):
        """Return a boolean representing variable instantiation."""
        return self[var].is_instantiated()

    def get_value(self, var: Var):
        """Return the current value of the variable."""
        return self[var].get_value()

    def check_consistency(self):
        """Throw an exception if one variable has an empty domain."""
        for var, domain in self.items():
            if domain.is_failed():
                raise Exception(f"FAILURE: {var} has an empty domain")


class Constraint:
    """A Constraint here is a propagator, that takes a State and modifies it."""

    def apply(self, state: State) -> bool:
        """Apply once the propagator on the state.

        return wether some domain has changed
        """
        raise NotImplementedError

    def is_entailed(self, state: State) -> bool:
        """Return wether the constraint is already entailed.

        by the current domains, and therefore will never propagate any more
        """
        return False


Constraints = List[Constraint]


class Model:
    """A Model is given by global domain limits, a State, a list of Constraints."""

    def __init__(self, dmin: int, dmax: int, variables: List[Var] = []):
        """Set stuff up."""
        self.state = State()
        self.stack: List[State] = []
        self.constraints: Constraints = []
        self.dmin = dmin
        self.dmax = dmax
        for var in variables:
            self.add_var(var)
        self.search_tree = None
        self.current_node = None
        self.next_node = None

    def plot_domains(self):
        """Pretty print using matplotlib."""
        plot_domains(self.state, self.dmin, self.dmax)

    def add_var(self, name: Var):
        """Add a variable."""
        self.state[name] = Domain(self.dmin, self.dmax)

    def add_constraint(self, constraint: Constraint):
        """Register a new constraint, but do not propagate it yet."""
        self.constraints.append(constraint)

    def propagate_once_each(self) -> bool:
        """Propagate once each constraint of the model.

        return a boolean indicating if some domain has changed.
        If after propagation a constraint is entailed, remove it
        finally, check if our Model is still consistent
        """
        to_remove: Constraints = []
        has_changed = False
        for constraint in self.constraints:
            has_changed |= constraint.apply(self.state)
            if constraint.is_entailed(self.state):
                to_remove.append(constraint)
        for constraint in to_remove:
            self.constraints.remove(constraint)
        self.state.check_consistency()
        return has_changed

    def propagate_all(self):
        """Propagate once, and continue as long as something changes."""
        while self.propagate_once_each():
            pass

    def solve(self, initial=True):
        """Propagate.

        If there are some non-instantiated variables, try to
        instantiate them to some value (and if that fails, try some other
        value)
        """
        if initial:
            self.search_tree = Digraph(graph_attr={"size": "14.0"})
            self.current_node = 0
            self.next_node = 1
            self.search_tree.node(str(self.current_node), "solve")
        self.propagate_all()
        try:
            for var in self.state.keys():
                if not self.state.is_instantiated(var):
                    self.stack.append(
                        (
                            copy.deepcopy(self.state),
                            copy.deepcopy(self.constraints),
                            self.current_node,
                        )
                    )
                    print(f"trying {var} = {self.state[var].current_min}")

                    self.search_tree.node(
                        str(self.next_node), f"{var} = {self.state[var].current_min}"
                    )
                    self.search_tree.edge(str(self.current_node), str(self.next_node))
                    self.current_node = self.next_node
                    self.next_node += 1

                    self.add_constraint(X_equal_C(var, self.state[var].current_min))
                    self.solve(False)
        except Exception as e:
            print(e)

            self.search_tree.node(
                str(self.next_node), label=str(e)[9:11], style="filled", color="red"
            )
            self.search_tree.edge(str(self.current_node), str(self.next_node))
            self.next_node += 1

            self.state, self.constraints, self.current_node = self.stack.pop()
            print(f"trying {var} != {self.state[var].current_min}")

            # self.search_tree.node(
            #     str(self.next_node),
            #     f'{var} != {self.state[var].current_min}')
            # self.search_tree.edge(str(self.current_node),
            #                       str(self.next_node))
            # self.current_node = self.next_node
            # self.next_node += 1

            self.add_constraint(X_different_from_C(var, self.state[var].current_min))
            self.solve(False)
        else:
            if initial:
                self.search_tree.node(
                    str(self.next_node), label="Success", style="filled", color="green"
                )
                self.search_tree.edge(str(self.current_node), str(self.next_node))


def plot_domains(state: State, dmin: int, dmax: int):
    """Display nicely in Jupyter notebooks."""
    variables = list(state.keys())
    domains = list(state.values())
    nvars = len(variables)

    fig, ax = plt.subplots()
    ax.grid(True, which="minor")
    ax.minorticks_on()
    xticks = range(dmin, dmax + 1, 1)
    mxticks = list(map(lambda x: x + 0.5, xticks[:-1]))
    ax.set_xticks(xticks)
    ax.set_xticks(mxticks, minor=True)

    yticks = range(0, nvars, 1)
    ax.set_yticks(yticks)
    myticks = list(map(lambda x: x + 0.5, yticks[:-1]))
    ax.set_yticks(myticks, minor=True)
    ax.set_yticklabels(variables)

    ax.xaxis.set_ticks_position("top")
    ax.imshow(
        [dom.as_bool() for dom in domains],
        cmap=plt.cm.binary,
        norm=None,
        filternorm=None,
        vmin=0,
        vmax=1,
        extent=(dmin - 0.5, dmax + 0.5, nvars - 0.5, -0.5),
    )


class X_different_from_C(Constraint):
    """Simple propagator for the constraint x != c."""

    def __init__(self, x: Var, c: int):
        self.x = x
        self.c = c

    def apply(self, state: State) -> bool:
        has_changed = self.c in state[self.x]
        if has_changed:
            state[self.x].remove_value(self.c)
        return has_changed


class X_equal_C(Constraint):
    """Simple propagator for the constraint x = c."""

    def __init__(self, x: Var, c: int):
        self.x = x
        self.c = c

    def apply(self, state: State) -> bool:
        has_changed = not state.get_value(self.x) == self.c
        state[self.x].current_min = self.c
        state[self.x].current_max = self.c
        return has_changed

    def is_entailed(self, state: State) -> bool:
        return state.get_value(self.x) == self.c

from DG import *

def contains_pattern(s,text):
    s = NFA(s)
    for i in range(len(text)):
        if (s.check_text(text[i:])):
            return True
    return False

class NFA:
    def __init__(self,s): # s is the string containing the regular expression
        self.s=s
        self.m=len(self.s)
        self.dg=DG(len(s)+1) # the directed graph that stores the epsilon links
        self.lp=[-1 for _ in range(len(s))]
        self.rp=[-1 for _ in range(len(s))]
        self.left_right_match_or() # assigns lp and rp according to parentheses matches
        self.build_eps_links() # assigns the epsilon links in self.dg

    def left_right_match(self):
        stack = []
        for i in range(self.m):
            if (self.s[i] == '('):
                stack.append(i)
            if (self.s[i] == ')'):
                l = stack.pop()
                self.rp[l] = i
                self.lp[i] = l

    def left_right_match_or(self):
        stack = []
        self.left_right_match()
        for i in range(self.m):
            if (self.s[i] == '('):
                stack.append(i)
            if (self.s[i] == ')'):
                l = stack.pop()
            if (self.s[i] == '|'):
                l = stack[len(stack) - 1]
                self.lp[i] = l
                self.rp[i] = self.rp[l]
  
    def build_eps_links(self):
        for i in range(self.m):
            if (self.s[i] == '|'):
                self.dg.add_link(self.lp[i], i + 1)
                self.dg.add_link(i, self.rp[i])
            if (self.s[i] == '(') or (self.s[i] == ')'):
                self.dg.add_link(i, i + 1)
            if (self.s[i] == '*'):
                self.dg.add_link(i, self.lp[i - 1])
                self.dg.add_link(self.lp[i - 1], i)
                self.dg.add_link(i, i + 1)

    def check_text(self,w):
        #complexity O(len(w) * self.m)
        start = [0]
        for S in w:
            now = self.dg.explore_from_subset(start)
            start = []
            for v in now:
                if (v == self.m):
                    return True
                if (self.s[v] == '.') or (self.s[v] == S):
                    start.append(v + 1)
                    if (v + 1 == self.m):
                        return True
            if (len(start) == 0):
                return False
        
        now = self.dg.explore_from_subset(start)
        for v in now:
            if v == self.m:
                return True
        return False

print(contains_pattern('c(.(a|b))*k(.)*','ck'))
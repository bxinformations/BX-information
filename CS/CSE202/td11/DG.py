class DG:
    def __init__(self,m):
        self.m=m
        self.neigh=[[] for _ in range(m)]

    def add_link(self,i,j):
        if not j in self.neigh[i]:
            self.neigh[i].append(j)

    def explore_from_subset(self,start_vertices): # returns the list of vertices that can be reached starting from a vertex in start_vertices, cf DFS exploration seen in CSE103
        visited=[False for _ in range(self.m)]
        for x in start_vertices:
            visited[x]=True
        stack=[x for x in start_vertices]
        res=[]
        while(len(stack)>0):
            x=stack.pop()
            for y in self.neigh[x]:
                if not visited[y]:
                    stack.append(y)
                    visited[y]=True
                res=[]
            for i in range(self.m):
                if visited[i]:
                    res.append(i)
        return res

    def to_string(self):
        res=""
        for i in range(self.m):
            res+=str(i)+" is linked to "+str(self.neigh[i])+"; "
        return res

# -*- coding: utf-8 -*-

import tkinter as tk

battleship.MISS = 'MISS'
battleship.HIT = 'HIT'
battleship.DESTROYED = 'DESTROYED'


class Battleship(tk.Frame):
    def __init__(self, player_grid, opponent_grid):
        root = tk.Tk()
        super().__init__(root)
        root.protocol("WM_DELETE_WINDOW", root.destroy)
        self.pack()
        self.player_grid = player_grid
        self.opponent_grid = opponent_grid
        self.scale = 20
        self.strategy = random_shoot

        self.create_widgets()

    def create_widgets(self):
        self.canvas1 = tk.Canvas(self, width=self.player_grid.sizex * self.scale, height=self.player_grid.sizey * self.scale, bg='white')
        self.canvas1.pack()
        self.show_grid_player()

        self.canvasmiddle = tk.Canvas(self, width=self.scale, height=self.scale)
        self.canvasmiddle.pack()

        self.canvas2 = tk.Canvas(self, width=self.opponent_grid.sizex * self.scale, height=self.opponent_grid.sizey * self.scale, bg='white')
        self.canvas2.bind("<Button-1>", self.shoot)
        self.canvas2.pack()
        self.show_grid_opponent()

    def shoot(self, event):
        pointx = event.x // self.scale
        pointy = event.y // self.scale
        shot_before = any((pointx,pointy) in ship.positions for ship in self.opponent_grid.ships)
        res, ship = self.opponent_grid.shoot((pointx,pointy))
        if res == battleship.MISS and not shot_before:
            self.canvas2.create_oval(pointx * self.scale+1, pointy * self.scale+1, (pointx +1)* self.scale-1, (pointy +1) * self.scale-1, fill= 'blue')
        else:
            self.canvas2.create_oval(pointx * self.scale+1, pointy * self.scale+1, (pointx +1)* self.scale-1, (pointy +1) * self.scale-1, fill= 'red')
        if ship is not None:
            self.show_sunk(ship)

        self.let_opponent_shoot()

    def let_opponent_shoot(self):
        b = battleship.BlindGrid(self.player_grid)
        x,y = self.strategy(b)
        if (x, y) in b.misses.union(b.hits):
            return # repeated shot, so nothing to do
        res, ship = self.player_grid.shoot((x,y))
        if res == battleship.MISS:
            self.canvas1.create_oval(x * self.scale+1, y * self.scale+1, (x +1)* self.scale-1, (y +1) * self.scale-1, fill= 'blue')
        else:
            self.canvas1.create_oval(x * self.scale+1, y * self.scale+1, (x +1)* self.scale-1, (y +1) * self.scale-1, fill= 'yellow')

    def show_sunk(self, ship):
        for x, y in ship.positions:
            self.canvas2.create_rectangle(x * self.scale, y * self.scale,
                                                   (x + 1) * self.scale -1,
                                                   (y + 1) * self.scale -1,
                                                   fill= 'red', outline='#D3D3D3')

    def show_alive(self):
        for rect in self.alive:
            self.canvas.delete(rect)
        points = self.board.points
        self.alive = {self.canvas.create_rectangle(point.x * self.scale, point.y * self.scale,
                                                   (point.x + 1) * self.scale -1,
                                                   (point.y + 1) * self.scale -1,
                                                   fill= 'black', outline='#D3D3D3')
                                            for point in points}
    def show_grid_player(self):
        for i in range(1, self.player_grid.sizex):
            self.canvas1.create_line(i * self.scale, 0, i * self.scale, self.player_grid.sizey * self.scale, fill = '#D3D3D3')
        for j in range(1, self.player_grid.sizey):
            self.canvas1.create_line(0, j * self.scale, self.player_grid.sizex * self.scale, j * self.scale, fill = '#D3D3D3')
        for ship in self.player_grid.ships:
            for x, y in ship.positions:
                self.canvas1.create_rectangle(x * self.scale, y * self.scale,
                                                   (x + 1) * self.scale -1,
                                                   (y + 1) * self.scale -1,
                                                   fill= 'red', outline='#D3D3D3')

    def show_grid_opponent(self):
        for i in range(1, self.opponent_grid.sizex):
            self.canvas2.create_line(i * self.scale, 0, i * self.scale, self.opponent_grid.sizey * self.scale, fill = '#D3D3D3')
        for j in range(1, self.opponent_grid.sizey):
            self.canvas2.create_line(0, j * self.scale, self.opponent_grid.sizex * self.scale, j * self.scale, fill = '#D3D3D3')

import random
def random_shoot(blind_grid):
    return random.randint(0, blind_grid.sizex), random.randint(0,blind_grid.sizey)

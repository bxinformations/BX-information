# -*- coding: utf-8 -*-

import tkinter as tk
import tkinter.filedialog as fdialog

class GraphicalLife(tk.Frame):
    def __init__(self, board, scale = 10):
        root = tk.Tk()
        super().__init__(root)
        root.protocol("WM_DELETE_WINDOW", root.destroy)
        self.pack()
        self.board = board
        self.scale = scale
        self.alive = set()

        self.create_widgets()

    def create_widgets(self):
        #self.create_next_button()
        #self.create_save_button()
    
        self.canvas = tk.Canvas(self, width=self.board.sizex * self.scale, height=self.board.sizey * self.scale, bg='white')
        #self.canvas.bind("<Button-1>", self.toggle_point)
        self.canvas.pack()
        self.show_alive()
        
    def create_next_button(self):
        self.next_button = tk.Button(self)
        self.next_button["text"] = "next turn"
        self.next_button["command"] = self.next_turn
        self.next_button.pack(side="bottom")

    def create_save_button(self):
        self.save_button = tk.Button(self)
        self.save_button["text"] = "save"
        self.save_button["command"] = self.save
        self.save_button.pack(side="bottom")
        
    def next_turn(self):
        self.board.next_step()
        self.show_alive()

    def toggle_point(self, event):
        pointx = event.x // self.scale
        pointy = event.y // self.scale
        self.board.toggle_point(pointx, pointy)
        self.show_alive()
        
    def save(self):
        f = fdialog.asksaveasfile(mode='w', defaultextension=".lf", filetypes = (("Life files", '*.lf'),))
        if f is None: # asksaveasfile return `None` if dialog closed with "cancel".
            return
        else:
            self.board.save_to_file(f.name)

    def show_alive(self):
        for rect in self.alive:
            self.canvas.delete(rect)
        #points = self.board.get_points()
        points = self.board.points
        self.alive = {self.canvas.create_rectangle(point.x * self.scale, point.y * self.scale, 
                                                   (point.x + 1) * self.scale -1, 
                                                   (point.y + 1) * self.scale -1, 
                                                   fill= 'black', outline='#D3D3D3') 
                                            for point in points}


            
        

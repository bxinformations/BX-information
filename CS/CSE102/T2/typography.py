#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Feb 27 08:09:00 2020

@author: peixin.you
"""

import sys
import PyQt5.QtCore as QtCore
import PyQt5.QtGui as QtGui
import PyQt5.QtWidgets as QtWidgets

class Box:
    def __init__(self, width, ascent, descent, stretching):
        self.__width = width
        self.__ascent = ascent
        self.__descent = descent
        self.__stretching = stretching

    @property
    def width(self):
        return self.__width

    @property
    def ascent(self):
        return self.__ascent
    
    @property
    def descent(self):
        return self.__descent

    @property
    def stretching(self):
        return self.__stretching

    def __str__(self):
        return "[w={w}, a={a}, d={d}, sc={s}]" \
            .format(w = self.width     ,
                    a = self.ascent    ,
                    d = self.descent   ,
                    s = self.stretching)
    
    def draw(self, painter, x, y, width):
        pass
    
    DEBUG = True

    def paint(self, painter, x, y, w):
        if self.DEBUG:
            painter.save()
            painter.setPen(QtCore.Qt.red)
            painter.setBrush(QtCore.Qt.transparent)
            painter.drawRect(x, y, w, int(self.ascent + self.descent))
            painter.restore()
            self.draw(painter, x, y, w)
    
class Glyph(Box):
    def __init__(self, font, c):
        self.__font = font
        self.__char = c
        metrics = QtGui.QFontMetrics(font)
        super().__init__(metrics.width(self.__char), metrics.ascent(), 
                       metrics.descent(), 0.0)
    
    @property
    def char(self):
        return self.__char
    
    @property
    def font(self):
        return self.__font
    
    def __str__(self):
        return "Glyph({c})".format(self.__class__.__name__,
                    c = self.__char) + super().__str__()
            
    def draw(self, painter, x, y, width):
        painter.setFont(self.font)
        painter.drawText(x, y + self.ascent, self.char)

class Space(Box):
    def __init__(self, font, width, stretching):
        self.__font = font
        super().__init__(width, 0, 0, stretching)
    
    @property
    def font(self):
        return self.__font
    
    def __str__(self):
        return "Space" + super().__str__()
    
    def draw(self, painter, x, y):
        pass

class FixedSpace(Space):
    def __init__(self, font):
        super().__init__(font, QtGui.QFontInfo(font).pointSizeF(), 0.0)
        
class RelativeSpace(Space):
    def __init__(self, c, font):
        super().__init__(font, c * QtGui.QFontInfo(font).pointSizeF(), 1.0)

class Group(Box):
    def __init__(self):
        self._boxes = []
        self.__width, self.__ascent, self.__descent, self.__stretching = 0, 0, 0, 0.0

    @property
    def width(self):
        return self.__width

    @property
    def ascent(self):
        return self.__ascent
    
    @property
    def descent(self):
        return self.__descent

    @property
    def stretching(self):
        return self.__stretching

    @property
    def boxes(self):
        return self._boxes

    def add(self, box):
        self._boxes.append(box)
        
    def __str__(self):
        boxes = ""
        for i in self._boxes:
            boxes += "    " + str(i) + "\n"
        return "[w={w}, a={a}, d={d}, sc={s}]{{" \
            .format(w = self.width     ,
                    a = self.ascent    ,
                    d = self.descent   ,
                    s = self.stretching) + "\n" \
                + boxes + "\n},"
    
    def draw(self, painter, x, y):
        pass
        
class HGroup(Group):
    def __init__(self):
        super().__init__()
        self.__width, self.__ascent, self.__decent, self.__stretching = 0, 0, 0, 0.0
    
    def add(self, box):
        self.__width += box.width
        self.__stretching += box.stretching
        super().add(box)
        
    def __str__(self):
        return "HGroup" + super().__str__()
    
    def draw(self, painter, x, y, width):
        nx = x
        for i in range(len(self.boxes)):
            print(i)
            self.boxes[i].draw(painter, nx, y, width)
            nx += self.boxes[i].width

class BoxDisplay(QtWidgets.QMainWindow):
    class BoxWidget(QtWidgets.QWidget):
        PADDING = 50

        def __init__(self, box, *args, **kwargs):
            super().__init__(*args, **kwargs)
            self.__box = box
            self.setContentsMargins(*([10] * 4))

        def sizeHint(self):
            return QtCore.QSize(
                2 * self.PADDING + self.__box.width,
                2 * self.PADDING + int(self.__box.ascent + self.__box.descent))
    
        def paintEvent(self, e):
            width, height = self.size().width(), self.size().height()
    
            with QtGui.QPainter(self) as painter:
                painter.setBrush(QtCore.Qt.black)
                painter.setPen(QtCore.Qt.black)
    
                painter.drawLine(self.PADDING, 0, self.PADDING, height)
                painter.drawLine(width - self.PADDING, 0, width - self.PADDING, height)
                painter.drawLine(0, self.PADDING, width, self.PADDING)
                painter.translate(self.PADDING, self.PADDING)
    
                try:
                    self.__box.paint(painter, 0, 0, self.width() - 2 * self.PADDING)
                except e:
                    print(e)

    def __init__(self, box, parent = None):
        super().__init__(parent)
        self._widget = BoxDisplay.BoxWidget(box = box, parent = self)
        self.setCentralWidget(self._widget)
        self.setWindowTitle('Typography')
        self.move(300, 300)
        self.setSizePolicy(QtWidgets.QSizePolicy.Preferred,
                           QtWidgets.QSizePolicy.Minimum)
        self.resize(self.sizeHint())

def _main_q3():
    app = QtWidgets.QApplication(sys.argv)
    fnt = QtGui.QFont("Helvetica", 30)
    glh = Glyph(fnt, "g")
    wdg = BoxDisplay(glh)

    print(glh); wdg.show(); app.exec_()

def box_from_line(s):
    fnt = QtGui.QFont("Helvetica", 30)
    grp = [Glyph(fnt, x) for x in s.split()]
    grp = sum([[x, RelativeSpace(0.5, fnt)] for x in grp], [])[:-1]
    box = HGroup()

    for x in grp:
        box.add(x)

    return box

def _main_q7():
    app = QtWidgets.QApplication(sys.argv)
    fnt = QtGui.QFont("Helvetica", 30)
    wdg = BoxDisplay(box_from_line("Typographie sans peine"))

    wdg.show(); app.exec_()

_main_q7()
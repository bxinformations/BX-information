#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov  7 10:16:44 2019

@author: peixin.you
"""

class Trobble:
    """Trobbles: simplified digital pets.

    Data Attributes:
    name -- the Trobble's name.
    sex -- 'male' or 'female'.
    age -- an integer between 0 (dead) and 10 (full health) inclusive
    health -- a non-negative integer (0 is dead)
    hunger -- a non-negative integer (0 is not hungry)
    """
    def __init__(self, name, sex):
        self.name = name
        self.sex = sex
        self.health = 10
        self.age = 0
        self.hunger = 0
        
    def __str__(self):
        """Give a string representation of the Trobble object's status, in the form
        '_name_: _sex_, health _health_, hunger _hunger_, age _age_'
        where _name_, _sex_, _health_, _hunger_ and _age_ are the values of
        the data attributes with the same name.
        """
        return "{}: {}, health {}, hunger {}, age {}".format(
                self.name, self.sex, self.health, self.hunger, self.age)
    
    def next_turn(self):
        """End the turn for the instance and 
        recompute the attribute values for the next turn."""
        if self.health == 0:
            return
        self.age += 1
        self.hunger += self.age
        self.health -= self.hunger // 20
        if self.health < 0:
            self.health = 0
    
    def feed(self):
        """Feed the instance to decrease the hunger 
        by 25 with a minimum value of 0."""
        if self.hunger > 25:
            self.hunger -= 25
        else:
            self.hunger = 0

    def cure(self):
        """Increase the health of the instance by 5 up to the maximum of 10."""
        if self.health < 5:
            self.health += 5
        else:
            self.health = 10
        
    def is_alive(self):
        """Return True if the health of the instance is positive,
        otherwise False.
        """
        return self.health != 0
    
def get_name():
    return input('Please give your new Trobble a name: ')

def get_sex():
    sex = None
    while sex is None:
        prompt = 'Is your new Trobble male or female? Type "m" or "f" to choose: '
        choice = input(prompt)
        if choice == 'm':
            sex = 'male'
        elif choice == 'f':
            sex = 'female'
    return sex

def get_action(actions):
    while True:
        prompt = 'Type one of {} perform the action: '.format(', '.join(actions.keys()))
        action_string = input(prompt)
        if action_string not in actions:
            print('Unknown action!')
        else:
            return actions[action_string]
        
def play():
    name = get_name()
    sex = get_sex()
    trobble = Trobble(name, sex)
    actions = {'feed': trobble.feed, 'cure': trobble.cure}
    while trobble.is_alive():
        print('You have one Trobble named ' + str(trobble))
        action = get_action(actions)
        action()
        trobble.next_turn()
    print('Unfortunately, your Trobble {} has died at the age of {}'.format(
            trobble.name, trobble.age))

def mate(trobble1, trobble2, name_offspring):
    """Check if the given Trobbles can procreate and if so give back a new
    Trobble that has the sex of trobble1 and the name 'name_offspring'.
    Otherwise, return None.
    """
    if not (trobble1.is_alive() or trobble2.is_alive()):
        return None
    if (trobble1.age < 4 or trobble2.age < 4):
        return None
    if (trobble1.sex == trobble2.sex):
        return None
    return Trobble(name_offspring, trobble1.sex)
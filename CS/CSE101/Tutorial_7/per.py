#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Nov 14 10:18:13 2019

@author: peixin.you
"""

import random

class Card:
    """French playing cards.

    Class attributes:
    suit_names -- the four suits Clubs, Diamonds, Hearts, Spades
    rank_names -- the 13 ranks in each suit: Two--Ten, Jack, Queen, King, Ace
    Data attributes:
    suit, rank -- the Card's suit and rank, as indices into the lists above
    """
    suit_names = ["Clubs", "Diamonds", "Hearts", "Spades"]
    rank_names = ["Two", "Three", "Four", "Five", "Six", "Seven", "Eight",
             "Nine", "Ten", "Jack", "Queen", "King", "Ace"]
    
    def __init__(self, suit, rank):
         self.suit = suit
         self.rank = rank
        
    def __str__(self):
        """Returns a readable string representation."""
        return self.rank_names[self.rank] + ' of ' + self.suit_names[self.suit]
    
    def __eq__(self, other):
        return self.rank == other.rank and self.suit == other.suit
    
    def matching_card(self):
        """Return the card which matches self."""
        return Card(((((self.suit) % 2 == 0) * 2 + 1 + self.suit) % 4), self.rank)
    
class Deck:
    """A deck of Cards.

    Data attributes:
    cards -- a list of all Cards in the Deck
    """
    
    def creat_list(self):
        for i in range(4):
            for j in range(self.minrank, 13):
                now = Card(i, j)
                self.cards.append(now)
        self.cards.remove(self.cards[0])
    
    def __init__(self, minrank):
        self.minrank = minrank
        self.cards = [] 
        self.creat_list()
    
    def __str__(self):       
        return ', '.join([str(card) for card in self.cards])
    
    def pop(self):
        """Remove and return last card from deck."""
        result = self.cards[-1]
        self.cards.remove(result)
        return result
    
    def shuffle(self):
        """Shuffle the deck."""
        random.shuffle(self.cards)
        
class Player:
    """A player of the card game.

    Data attributes:
    name -- the name of the player
    cards -- a list of all the player's cards (their "hand")
    """ 
    
    def __init__(self, name):
        self.name = name
        self.cards = []
        
    def __str__(self):
        if self.cards == []:
            return 'Player {} has no cards'.format(self.name)
        else:
            result = []
            for i in self.cards:
                result.append(str(i))
            return 'Player {} has: {}'.format(self.name, ', '.join(result))
        
    def add_card(self, card):
        """Add card to player's hand."""
        self.cards.append(card)
    
    def num_cards(self):
        """Return number of cards in player's hand."""
        return len(self.cards)
    
    def remove_card(self, i):
        """Remove and return i'th card from player's hand."""
        return self.cards.pop(i)
    
    def remove_matches(self):
        """Remove all pairs of matching cards."""
        original_cards = self.cards[:]
        tot = 0
        for i in original_cards:
            g = i.matching_card()
            if i in self.cards and g in self.cards:
                print('Player {}: {} matches {}'.format(self.name, str(i), str(g)))
                tot += 1
                self.cards.remove(g)
                self.cards.remove(i)
        return tot

class CardGame:
    """A class for playing card games.

    Data attributes:
    players -- a list of Player objects which participate in the game
    deck -- a Deck of Cards used for playing
    numcards -- number of Cards in the game
    """

    def __init__(self, players, minrank = 0):
        """Data atributes initialisation."""
        self.deck = Deck(minrank)
        lis = []
        for p in players:
            lis.append(Player(p))
        self.players = lis
        self.numcards = len(self.deck.cards)
    
    def __str__(self):
        """String representing each player and all of their cards."""
        result = ''
        for i in self.players:
            result += str(i) + '\n'
        return result[:-1]
    
    def shuffle_deck(self):
        """Shuffle the game's deck."""
        self.deck.shuffle()

#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 12 10:18:20 2019

@author: peixin.you
"""

class Vote:
    """A single vote object.
    
    Data attributes:
    - preference_list: a list of the preferred parties for this voter,
      in descending order of preference.
    """
    
    def __init__(self, preference_list):
        self.preference_list = preference_list
        
    def __str__(self):
        if not self.preference_list:
            return 'Blank'
        return ' > '.join(self.preference_list)
    
    def __repr__(self):
        return 'Vote({})'.format(self.preference_list)
    
    def first_preference(self):
        if not self.preference_list:
            return None
        return self.preference_list[0]
    
    def preference(self, names):
        """Return the item in names that occurs first in the preference list,
        or None if no item in names appears.
        """
        for i in names:
            if i in self.preference_list:
                return i
        return None
    
class Election:
    """A basic election class.
    
    Data attributes:
    - parties: a list of party names
    - blank: a list of blank votes
    - piles: a dictionary with party names for keys
      and lists of votes (allocated to the parties) for values
    """
    
    def __init__(self, parties):
        self.parties = parties
        self.blank = []
        self.dead = []
        self.piles = {name:[] for name in self.parties}
        
    def add_vote(self, vote):
        """Append the vote to the corresponding pile."""
        if vote.first_preference() == None:
            self.blank.append(vote)
        else:
            self.piles[vote.first_preference()].append(vote)

    def status(self):
        """Return the current status of the election:
        a dictionary mapping each of the party names in the piles
        to the number of votes in their pile.
        """
        result = {name : len(self.piles[name]) for name in self.parties}
        return result
    
    def add_votes_from_file(self, filename):
        """Append each of the votes in the file to the correct pile."""
        with open(filename, 'r') as f:
            for line in f:
                self.add_vote(Vote(line.split()))
    
    def first_past_the_post_winner(self):
        """Return the winner of this election under
        the first-past-the-post system, or None if
        the election is tied.
        """
        mx = -1
        name = ''
        f = True
        result = self.status()
        for i in self.parties:
            if result[i] > mx:
                mx = result[i]
                name = i
                f = True
            elif result[i] == mx:
                f = False
        if f:
            return name
        return None
    
    def eliminate(self, party):
        """Remove the given party from piles, and redistribute its 
        votes among the parties not yet eliminated, according to 
        their preferences.  If all preferences have been eliminated, 
        then add the vote to the dead list.
        """
        self.parties.remove(party)
        for i in self.piles[party]:
            if i.preference(self.parties) == None:
                self.dead.append(i)
            else:
                self.piles[i.preference(self.parties)].append(i)
                
    def round_loser(self):
        """Return the name of the party to be eliminated from the next round."""
        result = self.status()
        mn = min(list(result.values()))
        name = 'zzzzzzzzzzzzzzz'
        for i in result.keys():
            if result[i] == mn:
                name = min(name, i)
        return name

    def preferential_winner(self):
        """Run a preferential election based on the current piles of votes,
        and return the winning party.
        """
        while (len(self.parties) > 1):
            self.eliminate(self.round_loser())
        return self.parties[0]
#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec  5 10:09:59 2019

@author: peixin.you
"""

class Account:
    """A basic savings account.
    Data attributes:
        owner - owners' name (a string)
        IBAN - the accounts' identification number (a string)
        balance - the current amount of money in the account (an integer)
    """

    def __init__(self, owner, IBAN, balance):
        self.owner = owner
        self.IBAN = IBAN
        self.balance = balance
    
    def __str__(self):
        # Return string containing owner, IBAN, and balance on separate lines.
        return 'Owner: {}\nIBAN: {}\nbalance: {}'.format(self.owner, self.IBAN, self.balance)

    def __eq__(self, other):
        # Bank accounts are equal if their IBANs match.
        I = self.IBAN
        return I == other.IBAN

    def deposit(self, amount):
        """Deposits a given amount to the account."""
        if amount < 1:
            return (False, 'Amount must be positive.')
        self.balance += amount
        return (True, self.balance)

    def withdraw(self, amount):
        """Withdraws a given amount from the account."""
        if amount < 1:
            return (False, 'Amount must be positive.')
        if self.balance - amount < 0:
            return (False, 'Insufficient funds.')
        self.balance -= amount
        return (True, self.balance)

class Bank:
    """A simple savings bank.
    Data attributes:
        name - the bank's name
        accounts - a dictionary of accounts, keyed by their IBANs
    """

    def __init__(self, name):
        self.name = name
        self.accounts = {} # A new bank has no accounts
        self.partner_banks = []

    def __str__(self):
        if not bool(self.accounts):
            return 'Bank {} has no accounts.'.format(self.name)
        result = 'Bank ' + self.name + ' has the following accounts:\n'
        for i in self.accounts:
            result += str(self.accounts[i]) + '\n\n'
        return result[:-2]
    
    def open_account(self, owner, IBAN, balance):
        """Create an account with the given information and add it to 
        the accounts dictionary of the bank.
        If there is already an account with the IBAN at the Bank, do not
        change the accounts dictionary, but return 'IBAN already taken!'.
        """
        if IBAN in self.accounts.keys():
            return 'IBAN already taken!'
        new_account = Account(owner, IBAN, balance)
        self.accounts[IBAN] = new_account

    def close_account(self, IBAN):
        """Close the account with the given IBAN, print the message
        'The account was closed.', and return its balance.
        If no account in the bank has this IBAN,
        then print 'Account not found!' and return None.
        """
        if IBAN not in self.accounts.keys():
            print('Account not found!')
            return None
        print('The account was closed.')
        result = self.accounts[IBAN]
        del self.accounts[IBAN]
        return result.balance
    
    def holds_account(self, IBAN):
        """True if an account with the given IBAN is held in this bank
        (False otherwise).
        """
        return IBAN in self.accounts.keys()
    
    def deposit_to(self, IBAN, amount):
        """Deposit the given amount to the account with the given IBAN,
        if it exists (otherwise, print a warning and return None).
        """
        if not self.holds_account(IBAN):
            return (False, 'IBAN not found.')
        return self.accounts[IBAN].deposit(amount)
    
    def withdraw_from(self, IBAN, amount):
        """Withdraw the given amount from the account with the given IBAN,
        if it exists (otherwise, print a warning and return None).
        """        
        if not self.holds_account(IBAN):
            return (False, 'IBAN not found.')
        return self.accounts[IBAN].withdraw(amount)
    
    def transfer(self, sender_IBAN, receiver_IBAN, amount):
        """Transfer amount from the sender's to the receiver's accounts."""
        if not self.holds_account(sender_IBAN):
            return 'Sender not found.'
        f = self.holds_account(receiver_IBAN)
        if f:
            goal = self
        for i in self.partner_banks:
            if i.holds_account(receiver_IBAN):
                f = True
                goal = i
        if not f:
            return 'Receiver not found.'
        if amount < 1:
            return 'Amount must be positive.'
        f, r = self.withdraw_from(sender_IBAN, amount)
        if not f:
            return r
        goal.deposit_to(receiver_IBAN, amount)
        return 'Transfer successful.'

    def link_bank(self, bank):
        """Add bank to the list of partner banks."""
        self.partner_banks.append(bank)

def create_bank_from_file(filename):
    """Create a bank with name and accounts specified by the given filename."""
    with open(filename, 'r') as f:
        name = f.readline()
        result = Bank(name[:-1])
        for line in f:
            op = line
            op = ' '.join(filter(lambda x: x, op.split(' ')))
            op = op.split(' ')
            result.open_account(op[0].strip(), op[1].strip(), int(op[2].strip()))
        return result

def total_balances(banks):
    """Returns a dictionary of owner:total pairs, where owner ranges
    over all account owners in every bank in the given list of banks,
    and total is the sum of all of that owner's account balances.
    """
    result = {}
    for i in banks:
        for j in i.accounts.values():
            if j.owner in result.keys():
                result[j.owner] += j.balance
            else:
                result[j.owner] = j.balance
    return result
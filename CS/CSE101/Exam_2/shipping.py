#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Dec 19 10:16:32 2019

@author: peixin.you
"""

class Transfer:
    """Data attributes:
        - name: a string describing the port name
        - date: (day, month)
    """
    
    def __init__(self, name, date):
        self.name = name
        self.date = date
    
    def __repr__(self):
        return 'Transfer({}, {})'.format(repr(self.name), repr(self.date))
    
    def __str__(self):
        d, m = self.date
        return '{} = {}/{}'.format(self.name, d, m)

    def is_after(self, other_date):
        """True if the Transfer's date is later or equal to the given date,
        False otherwise.
        """ 
        d, m = other_date
        dn, mn = self.date
        if (m < mn) or (m == mn and d <= dn):
            return True
        return False
    
    def is_before(self, other_date):
        """True if the Transfer's date is earlier or equal to the given date,
        False otherwise.
        """
        d, m = other_date
        dn, mn = self.date
        if (m > mn) or (m == mn and d >= dn):
            return True
        return False

class Ship:
    """Data attributes:
        - name: the name of the ship
        - ports: a dictionary mapping port names to transfers
        - containers: list of containers booked on this ship
          (in order of loading).  Initially empty.
    """
    
    def __init__(self, name, transfer_list):
        self.name = name
        self.containers = []
        self.ports = {}
        for transfer in transfer_list:
            self.ports[transfer.name] = transfer
            
    def __str__(self):
        return self.name
    
    def __repr__(self):
        return 'Ship({}, {})'.format(
                repr(self.name),
                [repr(transfer) for transfer in self.ports.values()])
    
    def add_container(self, container):
        """Adds container to the ship."""
        self.containers.append(container)

    def has_route(self, origin_port, destination_port):
        """If this ship travels from the origin port to the destination port,
        then return a tuple (departure_date, arrival_date),
        where departure_date is the date the ship leaves the departure port,
        and arrival_date is the date the ship arrives at the destination port.
        Otherwise, return None.
        """
        if (origin_port not in self.ports.keys()) or (destination_port not in self.ports.keys()):
            return None
        dd = self.ports[destination_port]
        do = self.ports[origin_port]
        if (dd.is_before(do.date)):
            return None
        return (do.date, dd.date)


def transfer_from_string(name_and_date_str):
    """Given a string in the form 'PORT NAME = DD/MM', construct and
    return a Transfer object with port PORT NAME and date (HH,MM).
    """
    name_and_date_str = name_and_date_str.split(' = ')
    name = name_and_date_str[0]
    date = name_and_date_str[1]
    date = date.split('/')
    return Transfer(name, (int(date[0]), int(date[1])))

class Transporter:
    """A class modelling ship companies.
        Data attributes:
        ships: dictionary mapping ship names to ships
        passages: dictionary mapping port names to names of passing ships
    """

    def __init__(self, name):
        self.name = name
        self.ships = {}
        self.passages = {}
    
    def add_ship(self, ship):
        """Add a ship to the ships dictionary, and also add its name
        to the list of names of ships serving each of the ports in
        the passages dictionary.
        """
        self.ships[ship.name] = ship
        for s in ship.ports.keys():
            if s in self.passages:
                self.passages[s].append(ship.name)
            else:
                self.passages[s] = [ship.name]
    
    def remove_ship(self, ship_name):
        """Remove the named ship from ships, and from passages,
        and return the ship object.  If not found, return None.
        (If removing the ship id from services results in an empty
        list, then the corresponding key is removed too.)
        """
        for ship in self.ships[ship_name].ports.keys():
            self.passages[ship].remove(ship_name)
            if self.passages[ship] == []:
                del self.passages[ship]
        result = self.ships[ship_name]
        del self.ships[ship_name]
        return result
        
    def load_ships_from_file(self, filename):
        """Load a series of ships from the data in the given file.
        Ships are specified one per line, in the format
            SHIP NAME, PORT_1 = DD1/MM1, ..., PORT = DDn/MMn
        """
        with open(filename, 'r') as f:
            for i in f:
                ships = i.split(', ')
                name = ships[0]
                ships.remove(ships[0])
                ships = Ship(name, [transfer_from_string(i) for i in ships])
                self.add_ship(ships)

    def find_ships(self, container):
        """Return a set of ship IDs that suit this container's journey."""
        if (container.departure.name not in self.passages) or (container.arrival.name not in self.passages):
            return set()
        result = set()
        for ship in self.ships.values():
            if (container.departure.name not in ship.ports.keys()) or (container.arrival.name not in ship.ports.keys()):
                continue
            if (ship.ports[container.departure.name].is_after(container.departure.date) and ship.ports[container.arrival.name].is_before(container.arrival.date)):
                result.add(ship.name)
        return result
    
    def best_ship(self, container):
        """Return the name of the ship with the best trip for the container:
        that is, the ship arriving first.  If there is a tie for first arrival,
        then the ship leaving the origin port last is preferred.
        """
        ships = self.find_ships(container)
        name = ''
        date1 = (0, 0)
        date = (1000, 1000)
        for ship in ships:
            if self.ships[ship].ports[container.arrival.name].is_before(date):
                d, m = self.ships[ship].ports[container.arrival.name].date
                dn, mn = date
                if d == dn and m == mn:
                    if self.ships[ship].ports[container.departure.name].is_after(date1):
                        name = ship
                        date1 = self.ships[ship].ports[container.departure.name].date
                else:
                    name = ship
                    date = self.ships[ship].ports[container.arrival.name].date
        return name
    
    def book_containers_from_file(self, filename):
        """For each container in the file, find the best ship for
        the container; add the container to that ship; and set the container's
        ticket to be the ID of that ship.  Return a list of the unsatisfied
        containers for which no such ship exists. 
        """
        result = []
        with open(filename, 'r') as f:
            for line in f:
                re = line.split(', ')
                re = Container(re[0], transfer_from_string(re[1]), transfer_from_string(re[2]))
                s = self.find_ships(re) 
                if s == set():
                    result.append(re)
                for i in s:
                    if re not in self.ships[self.best_ship(re)].containers:
                        self.ships[self.best_ship(re)].containers.append(re)
        return result
    
    def strike(self, ship_name):
        """Cancel the ship and rebook its containers on alternative routes.
        Return the set of orphaned containers that could not be rebooked.
        """
        ship = self.remove_ship(ship_name)
        result = []
        for re in ship.containers:
            s = self.find_ships(re) 
            if s == set():
                result.append(re)
            for i in s:
                if i not in self.ships[self.best_ship(re)].containers:
                    self.ships[self.best_ship(re)].containers.append(re)
        return result
    
class Container:
    """Data attributes:
        - name: name
        - departure: Transfer containing the station that they want
          to leave from, with the earliest date they can leave
        - arrival: Transfer containing the station that they want
          to arrive at, with the latest date that they can arrive
        - ship: a ship that the container will travel on (initially None)
    """
    
    def __init__(self, name, departure, arrival):
        self.name = name
        self.departure = departure
        self.arrival = arrival
        self.ship = None
    
    def __repr__(self):
        return 'Container({}, {}, {})'.format(
                repr(self.name),
                repr(self.departure),
                repr(self.arrival))
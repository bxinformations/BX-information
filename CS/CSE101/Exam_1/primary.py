#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 24 10:15:28 2019

@author: peixin.you
"""

def print_approvals(ratings):
    """Given a list of tuples (candidate, percentage), where candidate
    is a string and percentage is a number, print out a line
        candidate percentage%
    for every entry of the list, in the same order as the list entries.
    """
    for i in ratings:
        name, rate = i
        print(name + ' ' + str(rate) + '%')
        
def read_candidate_ids(primary_file):
    """Read the entries of the given primary file,
    construct the corresponding candidate IDs,
    and return them as a list.
    """
    names = []
    with open(primary_file) as inputFile:
        for line in inputFile:
            name = line[: -1]
            name = name.split('; ')
            names.append(name[1] + ' ' + name[0])
    return names

def filter_candidates(candidate_ids, opinion_file):
    """Compute the sublist of the candidate ID list containing 
    only those candidates for which there are opinion poll 
    results in the opinion file.
    """
    namesInOpinion = []
    with open(opinion_file) as inputFile:
        for line in inputFile:
            name = line[: -1]
            name = name.split(' - ')
            namesInOpinion.append(name[1])
    result = []
    for i in candidate_ids:
        if i in namesInOpinion:
            result.append(i)
    return result

def read_polls(candidate_ids, opinion_file_name):
    """Read the opinion poll results in the given file and put them into
    a dictionary whose keys are the poll names, and whose values are
    dictionaries mapping candidate keys to approval rating values.
    """
    result = {}
    with open(opinion_file_name) as inputFile:
        for line in inputFile:
            name = line[: -1]
            name = name.split(' - ')
            if name[1] not in candidate_ids:
                continue
            if name[0] in result.keys():
                result[name[0]][name[1]] = float(name[2])
            else:
                result[name[0]] = {name[1]: float(name[2])}
    return result

def opinions_by_candidate(polls):
    """Given a dictionary as constructed by read_polls(),
    compute a new dictionary that has the candidate IDs as keys,
    with the corresponding values being dictionaries mapping polls
    to approval ratings for the candidates.
    """
    result = {}
    for i in polls.keys():
        for j in polls[i].keys():
            if j in result.keys():
                result[j][i] = polls[i][j]
            else:
                result[j] = {i: polls[i][j]}
    return result

def average_approvals(candidate_polls):
    """Given a dictionary as constructed by opinions_by_candidate(),
    compute a new dictionary with candidate IDs as keys and their
    average approval ratings (across all polls) as values.
    """
    result = {}
    for i in candidate_polls.keys():
        sum = 0.0
        tot = 0
        for j in candidate_polls[i].keys():
            sum += candidate_polls[i][j]
            tot += 1
        sum /= float(tot)
        result[i] = sum
    return result

def top_outsider(all_polls, poll_name):
    """Given a dictionary as returned by read_polls and the name 
    of a single poll P, return the ID of the candidate with the
    highest average approval rating among all candidates who
    do not appear in the poll P.
    If there is no candidate in the other polls that does not appear in P,
    then None is returned.
    """
    name = ''
    f = False
    mx = 0.0
    candidate_polls = opinions_by_candidate(all_polls)
    avg = average_approvals(candidate_polls)
    for i in avg.keys():
        if i in all_polls[poll_name]:
            continue
        if mx < avg[i]:
            f = True
            mx = avg[i]
            name = i
    if f == False:
        return None
    return name

def distance_from_average(all_polls, poll_name):
    """Given a dictionary as returned by read_polls and the name 
    of a single poll P, return a dictionary whose keys are the 
    candidates covered by P and whose values are tuples (r, d), 
    where r is the approval rating of the candidate in the poll P,
    and d = r - a, where a is the average approval rating of the
    candidate in all *other* polls (not including P).
    Candidate that appear in P, but not any other polls,
    do not appear in the resulting dictionary for P.
    """
    result = {}
    candidate_polls = opinions_by_candidate(all_polls)
    avg = average_approvals(candidate_polls)
    for i in all_polls[poll_name]:
        if len(candidate_polls[i]) == 1:
            continue
        result[i] = all_polls[poll_name][i] - (avg[i] * len(candidate_polls[i]) - all_polls[poll_name][i]) / (len(candidate_polls[i]) - 1)
    return result
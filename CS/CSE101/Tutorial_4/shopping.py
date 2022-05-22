#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Oct 17 10:09:53 2019

@author: peixin.you
"""

def print_recipe(recipe):
    """Pretty print recipe, which is a dictionary whose keys are ingredients
    and whose values are their corresponding amounts.
    """
    data = ""
    for k in recipe.keys():
        data += k + ',' + str(recipe[k]) + '\n'
    return data
        
def read_recipe(recipe_name):
    """Read recipe file 'recipe_name', and return ingredients as a
    dictionary whose keys are ingredients and whose values are the
    corresponding amounts.
    """
    recipe = {}
    with open(recipe_name) as inputFile:
        for line in inputFile:
            parts = line.split(',')
            if len(parts) == 1:
                continue
            name = parts[0].strip()
            val = int(parts[1].strip())
            recipe[name] = val
    return recipe

def read_market(market_name):
    """Read recipe file 'recipe_name', and return ingredients as a
    dictionary whose keys are ingredients and whose values are the
    corresponding amounts.
    """
    market = {}
    with open(market_name) as inputFile:
        for line in inputFile:
            parts = line.split(',')
            if len(parts) == 1:
                continue
            name = parts[0].strip()
            val = int(parts[1].strip())
            market[name] = val
    return market

def write_recipe(recipe, recipe_name):
    """Write recipe to a file named recipe_name.
    """
    outputFile = open(recipe_name, "w")
    outputFile.write(print_recipe(recipe))
    
def read_fridge(fridge_name):
    """Read fridge file 'fridge_name', and return the ingredients
    held in the given fridge as an ingredient=amount dictionary.
    """
    fridge = {}
    with open(fridge_name) as inputFile:
        for line in inputFile:
            parts = line.split(',')
            if len(parts) == 1:
                continue
            name = parts[0].strip()
            val = int(parts[1].strip())
            if (name in fridge.keys()):
                fridge[name] += val
            else:
                fridge[name] = val
    return fridge

def is_cookable(recipe_name, fridge_name):
    """Return True if the contents of the fridge named fridge_name
    are sufficient to cook the recipe named recipe_name.
    """
    fridge = {}
    fridge = read_fridge(fridge_name)
    recipe = {}
    recipe = read_recipe(recipe_name)
    f = True
    for k in recipe.keys():
        if k not in fridge.keys():
            f = False
            break
        if fridge[k] < recipe[k]:
            f = False
            break
    return f

def add_recipes(recipes):
    """Return a dictionary representing the sum of all of
    the recipe dictionaries in recipes.
    """
    result = {}
    for i in range(len(recipes)):
        for k in recipes[i].keys():
            if k in result.keys():
                result[k] += recipes[i][k]
            else:
                result[k] = recipes[i][k]
    return result

def create_shopping_list(recipe_names, fridge_name):
    """Return the shopping list (a dictionary of ingredients and amounts)
    needed to cook the recipes named in recipe_names, after the ingredients
    already present in the fridge named fridge_name have been used.
    """
    recipe = {}
    for i in range(len(recipe_names)):
        recipeNow = read_recipe(recipe_names[i])
        recipe = add_recipes([recipe, recipeNow])
    fridge = read_fridge(fridge_name)
    result = {}
    for k in recipe.keys():
        if k not in fridge.keys():
            result[k] = recipe[k]
        elif fridge[k] < recipe[k]:
            result[k] = recipe[k] - fridge[k]
    return result

def total_price(shopping_list, market_name):
    """Return the total price in millicents of the given shopping_list
    at the market named market_name.
    """
    market = read_market(market_name)
    tot = 0
    for k in shopping_list.keys():
        tot += market[k] * shopping_list[k]
    return tot

def find_cheapest(shopping_list, market_names):
    """Return the name of the market in market_names
    offering the lowest total price for the given shopping_list,
    together with the total price.
    """
    name = ''
    tot = 200012010000
    for i in range(len(market_names)):
        tmp = total_price(shopping_list, market_names[i])
        if tmp < tot:
            tot = tmp
            name = market_names[i]
    return (name, tot)

def update_fridge(fridge_name, recipe_names, market_names, new_fridge_name):
    """Compute the shopping list for the given recipes after the ingredients
    present in fridge fridge_name have been used; find the cheapest market;
    and write the new fridge contents to new_fridge_name.
    Print the shopping list, the cheapest market name, and the total amount
    to be spent at that market.
    """
    shopping_list = create_shopping_list(recipe_names, fridge_name)
    name, tot = find_cheapest(shopping_list, market_names)
    result = 'Shopping list:\n'
    for k in shopping_list.keys():
        result += k + ': ' + str(shopping_list[k]) + '\n'
    result += 'Market: ' + name + '\n'
    result += 'Total cost: ' + str(tot)
    print(result)
    newFridge = add_recipes([shopping_list, read_fridge(fridge_name)])
    write_recipe(newFridge, new_fridge_name)
    
def distributed_shopping_list(shopping_list, market_names):
    """Distribute shopping_list across the markets named in in market_names
    to minimize the total cost.
    """
    result = [{} for i in range(len(market_names))]
    for i in range(len(market_names)):
        market = {}
        market = read_market(market_names[i])
        for k in shopping_list.keys():
            f = True
            if k not in market.keys():
                continue
            for j in range(i):
                if k in result[j].keys():
                    f = False
                    if result[j][k] > market[k]:
                        del result[j][k]
                        result[i][k] = market[k]
            if f == True and k in market.keys():     
                result[i][k] = market[k]
    for i in range(len(market_names)):
        for k in result[i].keys():
            result[i][k] = shopping_list[k]
    print(result)
    ans = {}
    for i in range(len(market_names)):
        ans[market_names[i]] = result[i]
    return ans
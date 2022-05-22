#include <iostream>
#include <algorithm>
#include "td7.hpp"



std::vector<int> reverse_even(std::vector<int> input_vector)
{
    std::vector<int> ans;
    for (auto i : input_vector)
    {
        if ((i & 1) == 0)
        {
            ans.insert(ans.begin(), i);
        }
    }
    return ans;
}

std::vector<Coordinate> same_coordinates(std::vector<double> list_of_x,
                                          std::vector<double> list_of_y)
{
    std::vector<Coordinate> ans;
    for (auto i : list_of_x)
        for (auto j : list_of_y)
        {
            if (i == j)
            {
                ans.push_back(Coordinate(i, j));
            }
        }
    return ans;
}

std::vector<int> filter_max(std::vector<int> max_vector,
                            std::vector<int> other_vector)
{
    std::vector<int> ans;
    std::sort(max_vector.begin(),max_vector.end());
    std::sort(other_vector.begin(),other_vector.end());
    for (auto i : other_vector)
    {
        std::vector< int >::iterator iter=std::find(max_vector.begin(),max_vector.end(),i);
        if (iter == max_vector.end())
        {
            ans.push_back(i);
        }
    }
    return ans;
}

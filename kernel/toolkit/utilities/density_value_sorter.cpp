/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <list>         // std::list

#include "density_value_sorter.hpp"


tdx::utilities::DensityValueSorter::DensityValueSorter(int max_size, double* data)
{
    this->data = data;
    this->max_size = max_size;
}

void tdx::utilities::DensityValueSorter::sort_data_with_id()
{
    //Prepare the pairs
    std::list<DensityValueSorter::DensityIdPair> pairs;
    for(int id=0; id<max_size; id++)
    {
        pairs.push_back(DensityValueSorter::DensityIdPair(data[id], id));
    }
    
    //Sort the pairs
    pairs.sort();
    
    //Prepare the sorted data and id's
    sorted_data = (double*) malloc(max_size*sizeof(double));
    sorted_ids = (int*) malloc(max_size*sizeof(int));
    int id = 0;
    for (std::list<DensityValueSorter::DensityIdPair>::iterator pair=pairs.begin(); pair!=pairs.end(); ++pair)
    {
        sorted_data[id] = (*pair).density;
        sorted_ids[id] = (*pair).id;
        id++;
    }
    
}

int* tdx::utilities::DensityValueSorter::get_sorted_ids()
{
    sort_data_with_id();
    return sorted_ids;
}

double* tdx::utilities::DensityValueSorter::get_sorted_values()
{
    sort_data_with_id();
    return sorted_data;
}
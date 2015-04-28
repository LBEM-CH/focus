/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <stdexcept>
#include <string.h>

#include "real_space_data.hpp"
#include "../utilities/density_value_sorter.hpp"


namespace ds = volume::data;

ds::RealSpaceData::RealSpaceData()
{
    initialize(0, 0, 0);
}

ds::RealSpaceData::RealSpaceData(int nx, int ny, int nz)
{
    initialize(nx, ny, nz);
}

void ds::RealSpaceData::reset(int nx, int ny, int nz)
{
    _nx = nx;
    _ny = ny;
    _nz = nz;
    if(_data != NULL) delete[] _data;
    _data = (double*) calloc(nx*ny*nz, sizeof(double));
}

void ds::RealSpaceData::reset_data(double* data)
{
    if(_data != NULL) delete[] _data;
    _data = data;
}

double* ds::RealSpaceData::get_data() const
{
    return _data;
}

void ds::RealSpaceData::initialize(int nx, int ny, int nz)
{
    _nx = nx;
    _ny = ny;
    _nz = nz;
   // if(_data != NULL) delete[] _data;
    _data = (double*) calloc(nx*ny*nz, sizeof(double));
}

double ds::RealSpaceData::get_value_at(int x, int y, int z) const
{
    if(indices_in_limit(x, y, z))
    {
       return _data[memory_id(x, y, z)];
    }
    else
    {
        throw std::out_of_range
                ("ERROR! Fetching value got out of bound indices: " + std::to_string(x) + ", " + 
                  std::to_string(y) + ", " + std::to_string(z) + "\n");
    }
}

double ds::RealSpaceData::get_value_at(int id) const
{
    if(id < size())
    {
       return _data[id];
    }
    else
    {
        throw std::out_of_range
                ("ERROR! Fetching value got out of bound indices: " + std::to_string(id) + "\n");
    }
}

void ds::RealSpaceData::set_value_at(int x, int y, int z, double value)
{
    if(indices_in_limit(x, y, z))
    {
        _data[memory_id(x, y, z)] = value;
    }
    else
    {
        throw std::out_of_range
                ("ERROR! Setting value got out of bound indices: " + std::to_string(x) + ", " + 
                  std::to_string(y) + ", " + std::to_string(z) + "\n");
    }
}

void ds::RealSpaceData::set_value_at(int id, double value)
{
    if(id < size())
    {
       _data[id] = value;
    }
    else
    {
        throw std::out_of_range
                ("ERROR! Setting value got out of bound indices: " + std::to_string(id) + "\n");
    }
}


bool ds::RealSpaceData::indices_in_limit(int x, int y, int z) const
{
    bool result = true;
    if(x >= _nx || y >= _ny || z >= _nz)
    {
        result = false;
    }
    
    return result;
}

int ds::RealSpaceData::memory_id(int x, int y, int z) const
{
    return ( x + (y*_nx) + (z*_ny*_nx) );
}

int ds::RealSpaceData::nx() const
{
    return _nx;
}

int ds::RealSpaceData::ny() const
{
    return _ny;
}

int ds::RealSpaceData::nz() const
{
    return _nz;
}

double ds::RealSpaceData::min() const
{
    //Initialize min value
    double min = get_value_at(0);
    
    for(int i=1; i<size(); i++)
    {
        if(get_value_at(i) < min) min = get_value_at(i); 
    }
    return min;
}

double ds::RealSpaceData::max() const
{
    //Initialize max value
    double max = get_value_at(0);
    
    for(int i=1; i<size(); i++)
    {
        if(get_value_at(i) > max) max = get_value_at(i); 
    }
    return max;
}

double ds::RealSpaceData::mean() const
{
    double sum = 0.0;
    for(int i=1; i<size(); i++)
    {
        sum += get_value_at(i);
    }
    
    return sum/size();
}

long ds::RealSpaceData::size() const
{
    return _nx*_ny*_nz;
}

void ds::RealSpaceData::merge_data(const RealSpaceData& to_be_merged, int x, int y, int z)
{
    //Check the inputs
    if(indices_in_limit(x, y, z))
    {
        //Calculate the starting position
        int x_start = x - (int) to_be_merged.nx()/2;
        int y_start = y - (int) to_be_merged.ny()/2;
        int z_start = z - (int) to_be_merged.nz()/2;
        
        for(int ix=0; ix<to_be_merged.nx(); ix++ )
        {
            for(int iy=0; iy<to_be_merged.ny(); iy++)
            {
                for(int iz=0; iz<to_be_merged.nz(); iz++)
                {
                    int mx = x_start+ix;
                    int my = y_start+iy;
                    int mz = z_start+iz;
                    
                    if( mx>0 && mx<nx() && my>0 && my<ny() && mz>0 && mz<nz())
                    {
                        double current = get_value_at(mx, my, mz);
                        double new_value = to_be_merged.get_value_at(ix, iy, iz);
                        set_value_at(x_start+ix, y_start+iy, z_start+iz, current+new_value);
                    }
                }
            }
        }
        
    }
    else
    {
        throw std::out_of_range
                ("ERROR! merging data got out of bound indices: " + std::to_string(x) + ", " + 
                  std::to_string(y) + ", " + std::to_string(z) + "\n");
    }
}

int* ds::RealSpaceData::density_sorted_ids()
{
    volume::utilities::DensityValueSorter sorter(size(), _data);
    return sorter.get_sorted_ids();
}

double* ds::RealSpaceData::density_sorted_values()
{
    volume::utilities::DensityValueSorter sorter(size(), _data);
    return sorter.get_sorted_values();
}
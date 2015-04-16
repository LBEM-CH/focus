/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <stdexcept>
#include <string.h>
#include <string>

#include "real_space_data.hpp"


namespace ds = volume::data;

ds::RealSpaceData::RealSpaceData()
{
    this->initialize(0, 0, 0);
}

ds::RealSpaceData::RealSpaceData(int nx, int ny, int nz)
{
    this->initialize(nx, ny, nz);
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
        throw new std::out_of_range
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
        throw new std::out_of_range
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
        throw new std::out_of_range
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
        throw new std::out_of_range
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

long ds::RealSpaceData::size() const
{
    return _nx*_ny*_nz;
}
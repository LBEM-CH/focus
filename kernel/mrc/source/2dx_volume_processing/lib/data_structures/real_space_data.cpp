/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <stdexcept>
#include <string.h>
#include <math.h>
#include <cstring>

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

ds::RealSpaceData::RealSpaceData(const RealSpaceData& other)
{
    _nx = other._nx;
    _ny = other._ny;
    _nz = other._nz;
    _data = (double*) calloc(_nx*_ny*_nz, sizeof(double));
    std::memcpy(_data, other._data, _nx*_ny*_nz*sizeof(double));
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

void ds::RealSpaceData::vertical_slab(double height, double fraction, bool centered)
{
    if(height > 1.0)
    {
        std::cerr << "ERROR: Applying vertical density slab with slab height greater than volume z-length!";
        return;
    }
    
    //Check the fraction
    if(fraction < 0.0 || fraction > 1.0)
    {
        std::cerr << "ERROR! The density slab fraction can only be between 0 and 1";
        return;
    }
    
    int membrane_height = floor(height*nz());
    
    int density_start = (int) (nz()-membrane_height)/2;
    int density_end = density_start + membrane_height;
    int mid = (int) (nz()/2);
    
    if(centered) mid = 0;
    
    for ( int iz = 0; iz < nz(); ++iz ) 
    {
        int centered_iz = (iz + mid) % nz();
        if(centered_iz < density_start || centered_iz > density_end)
        {    
            /*
             * This is the region where density should be zero.
             * If the density is not centered, calculate the appropriate centered_iz
             */
            for ( int ix = 0; ix < nx(); ++ix ) 
            {
                for ( int iy = 0; iy < ny(); ++iy ) 
                {
                    
                    double new_density = get_value_at(ix, iy, centered_iz)*(1 - fraction);
                    set_value_at(ix, iy, centered_iz, new_density);

                }
            }
        }
    }
    
    
}

void ds::RealSpaceData::threshold(double limit, double fraction)
{
    std::cout << "Thresholding density with limit = " << limit << "\n";
    for(int ix=0; ix < nx(); ix++)
    {
        for(int iy=0; iy < ny(); iy++)
        {
            for(int iz=0; iz < nz(); iz++)
            {
                double density = get_value_at(ix, iy, iz);
                if(density < limit)
                {
                    set_value_at(ix, iy, iz, density*(1-fraction));
                }
            }
        }
    }
    
}

void ds::RealSpaceData::scale(double min, double max)
{
    double current_min = this->min();
    double current_max = this->max();
    
    std::cout << "Scaling densities in range (" << min << ", " << max << ")\n";
    
    double grad = (max-min)/(current_max-current_min);
    for(int ix=0; ix < nx(); ix++)
    {
        for(int iy=0; iy < ny(); iy++)
        {
            for(int iz=0; iz < nz(); iz++)
            {
                double density = get_value_at(ix, iy, iz);
                double new_density = (density - current_min)*grad + min;
                set_value_at(ix, iy, iz, new_density);
            }
        }
    }
    
}

void ds::RealSpaceData::grey_scale()
{
    scale(0, 255);
}

ds::RealSpaceData ds::RealSpaceData::binary_mask(double threshold)
{
    std::cout << "Creating binary mask with limit = " << threshold << "\n";
    
    RealSpaceData mask(nx(), ny(), nz());
    for(int ix=0; ix < nx(); ix++)
    {
        for(int iy=0; iy < ny(); iy++)
        {
            for(int iz=0; iz < nz(); iz++)
            {
                double density = get_value_at(ix, iy, iz);
                if(density < threshold)
                {
                    mask.set_value_at(ix, iy, iz, 0.0);
                }
                else
                {
                    mask.set_value_at(ix, iy, iz, 1.0);
                }
            }
        }
    }
    
    return mask;
}

void ds::RealSpaceData::apply_mask(const RealSpaceData& mask, double fraction)
{
    if(mask.nx() != nx() || mask.ny() != ny() || mask.nz() != nz())
    {
        std::cerr << "WARNING: Found different sizes for mask and volume. NOT MASKING!!!\n";
        return;   
    }
    
    std::cout << "Applying mask with fraction = " << fraction << "\n";
    for(int ix=0; ix < nx(); ix++)
    {
        for(int iy=0; iy < ny(); iy++)
        {
            for(int iz=0; iz < nz(); iz++)
            {
                double density = get_value_at(ix, iy, iz);
                double mask_density = mask.get_value_at(ix, iy, iz);
                if(mask_density <= 0.0)
                {
                    set_value_at(ix, iy, iz, density*(1-fraction));
                }
            }
        }
    }
}
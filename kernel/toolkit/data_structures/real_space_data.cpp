/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <stdexcept>
#include <string.h>
#include <math.h>
#include <algorithm>
#include <fftw3.h>

#include "real_space_data.hpp"
#include "../utilities/density_value_sorter.hpp"


namespace ds = tdx::data;

ds::RealSpaceData::RealSpaceData() : _data(NULL) {}

ds::RealSpaceData::RealSpaceData(int nx, int ny, int nz)
{
    _nx = nx;
    _ny = ny;
    _nz = nz;
    _data = new double[_nx*_ny*_nz]();
}

ds::RealSpaceData::RealSpaceData(const RealSpaceData& other)
{
    _nx = other._nx;
    _ny = other._ny;
    _nz = other._nz;
    _data = new double[_nx*_ny*_nz]();
    if(other._data != NULL) std::copy(other._data, other._data+(_nx*_ny*_nz), _data);
}

ds::RealSpaceData::~RealSpaceData()
{
    if(_data != NULL ) delete[] _data;
}

void ds::RealSpaceData::reset(const RealSpaceData& other)
{
    if( _data != NULL ) delete[] _data;
    _nx = other._nx;
    _ny = other._ny;
    _nz = other._nz;
    _data = new double[_nx*_ny*_nz]();
    if(other._data != NULL) std::copy(other._data, other._data+(_nx*_ny*_nz), _data);
}

void ds::RealSpaceData::clear()
{
    if(_data != NULL) delete _data;
    _data = new double[size()]();
}

void ds::RealSpaceData::set_from_fftw(double* fftw_data)
{
    for(int id=0; id<size(); ++id)
    {
        try
        {
            set_value_at(id, fftw_data[id]);
        }
        catch(std::exception e)
        {
            std::cerr << "Error while setting the element " << id << " from FFTW data\n";
            std::cerr << e.what();
            exit(1);
        }
    }
}

double* ds::RealSpaceData::get_data_for_fftw()
{
    double* fftw_data = fftw_alloc_real(size());
    for(int id=0; id<size(); ++id) fftw_data[id] = get_value_at(id); 
    return fftw_data;
}

ds::RealSpaceData& ds::RealSpaceData::operator=(const RealSpaceData& rhs)
{
    reset(rhs);
    return *this;
}

ds::RealSpaceData ds::RealSpaceData::operator+(const RealSpaceData& rhs) const
{
    RealSpaceData added =  RealSpaceData(nx(), ny(), nz());
    if( (rhs.nx() != nx()) || (rhs.ny() != ny()) || (rhs.nz() != nz()) )
    {
        std::cerr << "ERROR: Can't add real space data with different sizes\n\n";
        std::cerr << "RHS Size(" << rhs.nx() << ", " << rhs.ny() << ", " << rhs.nz() << ") does not match (" << nx() << ", " << ny() << ", " << nz() << ")\n";
        return *this;
    }
    
    for(int id=0; id < size(); id++)
    {
        added.set_value_at(id, get_value_at(id)+rhs.get_value_at(id));
    }
    
    return added;
}

ds::RealSpaceData ds::RealSpaceData::operator*(double factor) const
{
    RealSpaceData new_data =  RealSpaceData(nx(), ny(), nz());
    for(int id=0; id < size(); id++)
    {
        new_data.set_value_at(id, get_value_at(id)*factor);
    }
    
    return new_data;
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

size_t ds::RealSpaceData::memory_id(int x, int y, int z) const
{
    return ( x + (y*_nx) + (z*_ny*_nx) );
}

size_t ds::RealSpaceData::nx() const
{
    return _nx;
}

size_t ds::RealSpaceData::ny() const
{
    return _ny;
}

size_t ds::RealSpaceData::nz() const
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

size_t ds::RealSpaceData::size() const
{
    return _nx*_ny*_nz;
}

double ds::RealSpaceData::squared_sum() const
{
    double sum = 0.0;
    for(int i=1; i<size(); i++)
    {
        double density = get_value_at(i);
        sum += density*density;
    }
    
    return sum;
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
    tdx::utilities::DensityValueSorter sorter(size(), get_data_copy(0, size()-1));
    return sorter.get_sorted_ids();
}

double* ds::RealSpaceData::density_sorted_values()
{
    tdx::utilities::DensityValueSorter sorter(size(), get_data_copy(0, size() -1));
    return sorter.get_sorted_values();
}

double* ds::RealSpaceData::get_data_copy(int start, int end) const
{
    //Check for inputs
    if(start<0 || end >= size())
    {
        throw std::out_of_range("ERROR! Error while getting RealSpaceData - Indices provided are out of range!!\n");
    }
    
    double* data_copy = new double[size()];
    std::copy(_data+start, _data+(end-start), data_copy);
    
    return data_copy;
}

ds::RealSpaceData ds::RealSpaceData::vertical_slab_mask(double height, bool centered)
{
    RealSpaceData mask(nx(), ny(), nz());
    
    if(height > nz())
    {
        std::cerr << "ERROR: Applying vertical density slab with slab height greater than volume z-length(" << nz() << ")\n";
        return mask;
    }
    
    int membrane_height = floor(height);
    if(height <= 1.0) membrane_height = floor(height*nz());
    
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
                    mask.set_value_at(ix, iy, centered_iz, 0.0);
                }
            }
        }
        else
        {
            for ( int ix = 0; ix < nx(); ++ix ) 
            {
                for ( int iy = 0; iy < ny(); ++iy ) 
                {
                    mask.set_value_at(ix, iy, centered_iz, 1.0);
                }
            }
        }
    }
    
    return mask;
}

void ds::RealSpaceData::vertical_slab(double height, double fraction, bool centered)
{   
    //Check the fraction
    if(fraction < 0.0 || fraction > 1.0)
    {
        std::cerr << "ERROR! The density slab fraction can only be between 0 and 1";
        return;
    }
    
    RealSpaceData mask = vertical_slab_mask(height, centered);
    apply_mask(mask, fraction);
}

void ds::RealSpaceData::scale(double min, double max)
{
    double current_min = this->min();
    double current_max = this->max();
    
    std::cout << "Scaling densities to (" << min << ", " << max << ")\n";
    
    double grad = (max-min)/(current_max-current_min);
    for(int id=0; id < size(); id++)
    {
        double density = get_value_at(id);
        double new_density = (density - current_min)*grad + min;
        set_value_at(id, new_density);
    }
    
}

void ds::RealSpaceData::grey_scale()
{
    scale(0, 255);
}

ds::RealSpaceData ds::RealSpaceData::threshold_soft_mask(double th, double tl) const
{
    if(th < tl)
    {
        double tt = th;
        th = tl;
        tl = tt;
    }
    
    //Return normal threshold mask the difference is too low
    if(th-tl < 0.001)
    {
        return threshold_mask(th);
    }
    
    std::cout << "Creating threshold soft mask with limits = " << th << ", " << tl<< "\n";
    
    RealSpaceData mask(nx(), ny(), nz());
    for(int id=0; id < size(); id++)
    {
        double currentDensity = get_value_at(id);
        if(currentDensity >= th)
        {
            mask.set_value_at(id, 1.0);
        }
        else if(currentDensity < tl)
        {
            mask.set_value_at(id, 0.0);
        }
        else
        {
            double scaledDensity = (currentDensity - tl)/(th - tl); 
            mask.set_value_at(id, scaledDensity);
        }
    }
    
    return mask;
}

ds::RealSpaceData ds::RealSpaceData::threshold_mask(double threshold) const
{
    std::cout << "Creating threshold mask with limit = " << threshold << "\n";
    
    RealSpaceData mask(nx(), ny(), nz());
    for(int id=0; id < size(); id++)
    {
        if(get_value_at(id) >= threshold)
        {
            mask.set_value_at(id, 1.0);
        }
        else
        {
            mask.set_value_at(id, 0.0);
        }
    }
    
    return mask;
}

ds::RealSpaceData ds::RealSpaceData::threshold_below_mask(double threshold) const
{
    std::cout << "Creating threshold below mask with limit = " << threshold << "\n";
    
    RealSpaceData mask(nx(), ny(), nz());
    for(int id=0; id < size(); id++)
    {
        if(get_value_at(id) <= threshold)
        {
            mask.set_value_at(id, 1.0);
        }
        else
        {
            mask.set_value_at(id, 0.0);
        }
    }
    
    return mask;
}

void ds::RealSpaceData::threshold(double limit, double fraction)
{
    std::cout << "Thresholding density with limit = " << limit << "\n";
    RealSpaceData mask = threshold_mask(limit);
    apply_mask(mask, fraction);  
}

ds::RealSpaceData ds::RealSpaceData::dilate(double radius) const
{
    std::cout << "Dilating binary volume by radius " << radius << "\n";
    
    double rad2=radius * radius;
    RealSpaceData mask(nx(), ny(), nz());
    for(int ix=0; ix < nx(); ix++)
    {
        for(int iy=0; iy < ny(); iy++)
        {
            for(int iz=0; iz < nz(); iz++)
            {
                double density = get_value_at(ix, iy, iz);
                if(density > 0.5)
                {
                    for (int jx = ix-radius; jx<ix+radius+1; jx++) {
                        for (int jy = iy-radius; jy<iy+radius+1; jy++) {
                            for (int jz = iz-radius; jz<iz+radius+1; jz++) {
                                double lrad2 = (ix-jx)*(ix-jx) + (iy-jy)*(iy-jy) + (jz-iz)*(jz-iz);
                                if ( lrad2 < rad2 ) {
                                    mask.set_value_at(jx, jy, jz, 1.0);
                                }
                            }
                        }
                    }
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
    for(int id=0; id < size(); id++)
    {
        double density = get_value_at(id);
        double mask_density = mask.get_value_at(id);
        if(mask_density <= 0.0)
        {
            set_value_at(id, density*(1-fraction));
        }
    }
}

void ds::RealSpaceData::multiply_mask(const RealSpaceData& mask)
{
    if(mask.nx() != nx() || mask.ny() != ny() || mask.nz() != nz())
    {
        std::cerr << "WARNING: Found different sizes for mask and volume. NOT MASKING!!!\n";
        return;   
    }
    
    std::cout << "Multiplying mask with the real space data..\n";
    for(int id=0; id < size(); id++)
    {
        double density = get_value_at(id);
        double mask_density = mask.get_value_at(id);
        set_value_at(id, density*mask_density);
    }
}

ds::RealSpaceData ds::RealSpaceData::mask_applied_data(const RealSpaceData& mask, double fraction) const
{
    if(mask.nx() != nx() || mask.ny() != ny() || mask.nz() != nz())
    {
        std::cerr << "WARNING: Found different sizes for mask and volume. NOT MASKING!!!\n";
        return *this;   
    }
    
    std::cout << "Applying mask with fraction = " << fraction << "\n";
    
    RealSpaceData masked(nx(), ny(), nz());
    for(int id=0; id < size(); id++)
    {
        double density = get_value_at(id);
        double mask_density = mask.get_value_at(id);
        if(mask_density <= 0.0)
        {
            masked.set_value_at(id, density*(1-fraction));
        }
        else
        {
            masked.set_value_at(id, density);
        }
    }
    
    return masked;
}
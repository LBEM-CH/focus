/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <math.h>

#include "fourier_space_data.hpp"


namespace ds = volume::data;

ds::FourierSpaceData::FourierSpaceData()
{
    _data = new std::map<MillerIndex, DiffractionSpot>;
}

ds::FourierSpaceData::FourierSpaceData(const std::multimap<MillerIndex, DiffractionSpot>& spot_multimap)
{
    _data = new std::map<MillerIndex, DiffractionSpot>;
    
    bool initialized = false;
    
    ds::MillerIndex current_index;
    std::list<ds::DiffractionSpot> current_spots;
    
    for(std::map<ds::MillerIndex, ds::DiffractionSpot>::const_iterator spot_itr=spot_multimap.begin(); 
            spot_itr!=spot_multimap.end(); ++spot_itr)
    {
        // Initialize for the start
        if(!(initialized))
        {
            current_index = (*spot_itr).first;
            initialized = true;
        }
        
        // Average and insert accumulated spots
        if(!(current_index == (*spot_itr).first))
        {
            ds::DiffractionSpot avg_spot(current_spots);
            this->set_value_at(current_index.h(), current_index.k(), current_index.l(), avg_spot.value(), avg_spot.weight());
            current_spots.clear();
        }
        
        //Accumulate the spots in a list
        current_spots.push_back((*spot_itr).second);
        current_index = (*spot_itr).first;
        
    }
    
    //insert the final reflection
    ds::DiffractionSpot avg_spot(current_spots);
    this->set_value_at(current_index.h(), current_index.k(), current_index.l(), avg_spot.value(), avg_spot.weight());
}

void ds::FourierSpaceData::reset()
{
    _data->clear();
}

ds::FourierSpaceData::const_iterator ds::FourierSpaceData::begin() const
{
    return _data->begin();
}

ds::FourierSpaceData::const_iterator ds::FourierSpaceData::end() const
{
    return _data->end();
}

bool ds::FourierSpaceData::exists(int h, int k, int l) const
{
    bool result = true;
    if ( _data->find(MillerIndex(h, k, l)) == _data->end() )
    {
        result = false;
    }
    
    return result; 

}

double ds::FourierSpaceData::intensity_sum() const
{
    double sum = 0.0;
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {
        sum += (*ref).second.intensity();
    }
    
    return sum;
}

int ds::FourierSpaceData::spots() const
{
    return _data->size();
}

double ds::FourierSpaceData::max_amplitude() const
{
    double max = 0;
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {
        //Assign the current Miller Index to the array
        double amplitude = (*ref).second.value().amplitude();
        if(max < amplitude) max = amplitude;
    }
    
    return max;
}

void ds::FourierSpaceData::scale_amplitudes(double factor)
{
    std::cout << "Scaling the amplitude by a factor of: " << factor << "\n";
    FourierSpaceData* data = new FourierSpaceData();
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {
        ds::MillerIndex index = (*ref).first;
        Complex2dx currentComplex = (*ref).second.value();
        double current_amp = currentComplex.amplitude();
        currentComplex.set_amplitude(current_amp*factor);
        data->set_value_at(index.h(), index.k(), index.l(), currentComplex, (*ref).second.weight());
    }
    this->reset();
    this->_data = data->_data;
}

void ds::FourierSpaceData::set_value_at(int h, int k, int l, Complex2dx value, double weight)
{
    MillerIndex index = MillerIndex(h, k, l);
    //if(h < 0) std::cerr << "\nWARNING: Encountered negative h value, there is a problem somewhere!\n\n";
    if(exists(h,k,l)) _data->erase(index);
    if(value.amplitude() > 0.0001) _data->insert(std::pair<MillerIndex, DiffractionSpot>(index, DiffractionSpot(value, weight)));
}

ds::Complex2dx ds::FourierSpaceData::complex_at(int h, int k, int l) const
{
    ds::Complex2dx value(0.0, 0.0);
    if(exists(h,k,l))
    {
        value = _data->at(MillerIndex(h, k, l)).value();
    }
    return value;
}

double ds::FourierSpaceData::weight_at(int h, int k, int l) const
{
    double weight = 0.0;
    if(exists(h,k,l))
    {
        weight = _data->at(MillerIndex(h, k, l)).weight();
    }
    return weight;
}

fftw_complex* ds::FourierSpaceData::fftw_data(int fx, int fy, int fz) const
{
    fftw_complex* fftw_data;
    fftw_data = (fftw_complex*) calloc(fx*fy*fz, sizeof(fftw_complex));

    
    //Iterate over all possible miller indices h, k, l
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {

        //Assign the current Miller Index to the array
        ds::MillerIndex currentHKL = (*ref).first;
        Complex2dx currentComplex = (*ref).second.value();
        
        // Fill in this spot
        if(currentHKL.h() >= 0){
            int idx = currentHKL.h();
            int idy = currentHKL.k();
            int idz = currentHKL.l();
            if(idy < 0) idy = idy + fy;
            if(idz < 0) idz = idz + fz;
            
            int memory_id = idx + (idy*fx) + (idz*fy*fx);  

            ((fftw_complex*)fftw_data)[memory_id][0] = currentComplex.real();
            ((fftw_complex*)fftw_data)[memory_id][1] = currentComplex.imag();
        }
    }
    
    return fftw_data;

}

void ds::FourierSpaceData::reset_data_from_fftw(int fx, int fy, int fz, fftw_complex* complex_data)
{
    this->reset();
    
    int h_max = fx - 1;
    int k_max = (int) fy/2;
    int l_max = (int) fz/2;
           

    //Loop over all possible indices and set their values
    for(int ix=0; ix < fx; ix++){
        for(int iy=0; iy < fy; iy++){
            for(int iz=0; iz<fz ; iz++){
                int memory_id = ix + (iy*fx) + (iz*fy*fx); 
                double real = ((fftw_complex*)complex_data)[memory_id][0];
                double imag = ((fftw_complex*)complex_data)[memory_id][1];

                //Assign the current miller index
                Complex2dx currentComplex(real, imag);
                
                int h = ix;
                int k = iy;
                int l = iz;
                
                if(k > k_max) k = k - fy;
                if(l > l_max) l = l - fz;

                MillerIndex currentHKL(h, k, l);

                if(h >= 0 && h <= h_max &&  currentComplex.amplitude() > 0.0001){
                    this->set_value_at(currentHKL.h(), currentHKL.k(), currentHKL.l(), currentComplex, 1.0);
                }
            }
        }
    }
}

void ds::FourierSpaceData::replace_reflections(const FourierSpaceData& input, double fraction)
{
    std::cout << "Replacing the reflections from a known set..\n";
    FourierSpaceData new_data;
    
    //Iterate over all possible miller indices h, k, l
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {

        //Assign the current Miller Index to the array
        ds::MillerIndex index = (*ref).first;
        ds::Complex2dx current_complex = (*ref).second.value();
        ds::Complex2dx new_complex(current_complex.real(), current_complex.imag());
        
        if(input.complex_at(index.h(), index.k(), index.l()).amplitude() > 0.0001)
        {
            Complex2dx input_complex = input.complex_at(index.h(), index.k(), index.l());
            new_complex = input_complex*fraction + current_complex*(1-fraction);
        }
        
        new_data.set_value_at(index.h(), index.k(), index.l(), new_complex, weight_at(index.h(), index.k(), index.l()) );
    }
    
    this->reset();
    this->_data = new_data._data;
    
}

ds::FourierSpaceData ds::FourierSpaceData::invert_hand() const
{
    //Iterate over all possible miller indices h, k, l
    FourierSpaceData new_data;
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {
        //Assign the current Miller Index to the array
        ds::MillerIndex currentHKL = (*ref).first;
        new_data.set_value_at(currentHKL.h(), currentHKL.k(), -1*currentHKL.l(), (*ref).second.value(), (*ref).second.weight());
    }
    
    return new_data;
}

void ds::FourierSpaceData::spread_data()
{
    //TODO
    
}

ds::FourierSpaceData ds::FourierSpaceData::get_full_fourier() const
{
    
    FourierSpaceData full_data;
    for(const_iterator ref=this->begin(); ref!=this->end(); ++ref)
    {
        //Assign the current Miller Index to the array
        ds::MillerIndex currentHKL = (*ref).first;
        ds::Complex2dx currentComplex = (*ref).second.value();
        
        ds::MillerIndex friedelHKL = currentHKL.FriedelSpot();
        ds::Complex2dx friedelComplex = currentComplex;
        friedelComplex.set_phase(-1*currentComplex.phase());
        
        full_data.set_value_at(currentHKL.h(), currentHKL.k(), currentHKL.l(), currentComplex, (*ref).second.weight());
        full_data.set_value_at(friedelHKL.h(), friedelHKL.k(), friedelHKL.l(), friedelComplex, (*ref).second.weight());
    }
    
    return full_data;
    
}
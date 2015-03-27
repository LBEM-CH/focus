/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include "fourier_space_data.hpp"


namespace ds = volume_processing_2dx::data_structures;

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

void ds::FourierSpaceData::set_value_at(int h, int k, int l, Complex2dx value, double weight)
{
    MillerIndex index = MillerIndex(h, k, l);
    if(exists(h,k,l)) _data->erase(index);
    _data->insert(std::pair<MillerIndex, DiffractionSpot>(index, DiffractionSpot(value, weight)));
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
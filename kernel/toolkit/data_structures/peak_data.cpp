/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "peak_data.hpp"
#include "../utilities/fom_utilities.hpp"


namespace ds = tdx::data;

ds::PeakData::PeakData()
{
    initialize(Complex(0.0, 0.0), 0.0);
}

ds::PeakData::PeakData(Complex value, double weight)
{
    initialize(value, weight);
}

void ds::PeakData::from_peak_list(const ds::PeakList peaks)
{
    tdx::Complex sum_values;
    std::list<double> foms;
    double sum_foms = 0.0;
    
    for(ds::PeakList::const_iterator peaks_itr = peaks.begin(); 
                peaks_itr != peaks.end(); ++peaks_itr)
    {
        foms.push_back((*peaks_itr).weight());
        sum_values = sum_values + (*peaks_itr).value();
        sum_foms = sum_foms + (*peaks_itr).weight();
    }
    
    //Get the averaged weight
    double avg_weight = tdx::utilities::fom_utilities::AverageFOMs(foms);
    
    //The averaged value
    tdx::Complex avg_value = sum_values*(avg_weight/sum_foms);
    
    this->initialize(avg_value, avg_weight);
}

void ds::PeakData::initialize(Complex value, double weight)
{
    set_value(value);
    set_weight(weight);
}

ds::PeakData& ds::PeakData::operator =(const PeakData& rhs)
{
    this->initialize(rhs.value(), rhs.weight());
    return *this;
}

ds::PeakData ds::PeakData::operator +(const PeakData& rhs)
{
    std::list<double> weights;
    weights.push_back(this->weight());
    weights.push_back(rhs.weight());
    return PeakData(this->value()+rhs.value(), 
                           tdx::utilities::fom_utilities::AverageFOMs(weights) 
                          );
}

ds::PeakData ds::PeakData::operator *(double factor)
{
    return ds::PeakData(value()*factor, weight());
}

bool ds::PeakData::operator ==(const PeakData& rhs) const
{
    return (this->value() == rhs.value() && this->weight() == rhs.weight());
}

bool ds::PeakData::operator <(const PeakData& rhs) const
{
    return ( (this->value() == rhs.value() && this->weight() > rhs.weight()) ||
             (this->value() < rhs.value())
           );
}

tdx::Complex ds::PeakData::value() const
{
    return _value;
}

double ds::PeakData::weight() const
{
    return _weight;
}

double ds::PeakData::amplitude() const
{
    return value().amplitude();
}

double ds::PeakData::phase() const
{
    return value().phase();
}

double ds::PeakData::intensity() const
{
    return value().intensity();
}

void ds::PeakData::set_value(Complex value)
{
    this->_value = value;
}

void ds::PeakData::set_weight(double weight)
{
    if(weight < 0.0 || weight > 1.0)
    {
        throw new std::invalid_argument
                ("Error in setting weight, expected between 0 and 1 found: " + std::to_string(weight));
    }   
    else
    {
        this->_weight = weight;
    }
}

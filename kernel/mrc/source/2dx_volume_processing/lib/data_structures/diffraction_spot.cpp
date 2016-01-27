/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "diffraction_spot.hpp"
#include "common_definitions.hpp"
#include "../utilities/fom_utilities.hpp"


namespace ds = tdx::data;

ds::DiffractionSpot::DiffractionSpot()
{
    initialize(Complex(0.0, 0.0), 0.0);
}

ds::DiffractionSpot::DiffractionSpot(Complex value, double weight)
{
    initialize(value, weight);
}

ds::DiffractionSpot::DiffractionSpot(const std::list<DiffractionSpot> spots)
{
    ds::Complex sum_values;
    std::list<double> foms;
    double sum_foms = 0.0;
    
    for(ds::DiffractionSpotList::const_iterator spots_itr = spots.begin(); 
                spots_itr != spots.end(); ++spots_itr)
    {
        foms.push_back((*spots_itr).weight());
        sum_values = sum_values + (*spots_itr).value();
        sum_foms = sum_foms + (*spots_itr).weight();
    }
    
    //Get the averaged weight
    double avg_weight = tdx::utilities::fom_utilities::AverageFOMs(foms);
    
    //The averaged value
    ds::Complex avg_value = sum_values*(avg_weight/sum_foms);
    
    this->initialize(avg_value, avg_weight);
}

void ds::DiffractionSpot::initialize(Complex value, double weight)
{
    set_value(value);
    set_weight(weight);
}

ds::DiffractionSpot& ds::DiffractionSpot::operator =(const DiffractionSpot& rhs)
{
    this->initialize(rhs.value(), rhs.weight());
    return *this;
}

ds::DiffractionSpot ds::DiffractionSpot::operator +(const DiffractionSpot& rhs)
{
    std::list<double> weights;
    weights.push_back(this->weight());
    weights.push_back(rhs.weight());
    return DiffractionSpot(this->value()+rhs.value(), 
                           tdx::utilities::fom_utilities::AverageFOMs(weights) 
                          );
}

ds::DiffractionSpot ds::DiffractionSpot::operator *(double factor)
{
    return ds::DiffractionSpot(value()*factor, weight());
}

bool ds::DiffractionSpot::operator ==(const DiffractionSpot& rhs) const
{
    return (this->value() == rhs.value() && this->weight() == rhs.weight());
}

bool ds::DiffractionSpot::operator <(const DiffractionSpot& rhs) const
{
    return ( (this->value() == rhs.value() && this->weight() > rhs.weight()) ||
             (this->value() < rhs.value())
           );
}

ds::Complex ds::DiffractionSpot::value() const
{
    return _value;
}

double ds::DiffractionSpot::weight() const
{
    return _weight;
}

double ds::DiffractionSpot::amplitude() const
{
    return value().amplitude();
}

double ds::DiffractionSpot::phase() const
{
    return value().phase();
}

double ds::DiffractionSpot::intensity() const
{
    return value().intensity();
}

void ds::DiffractionSpot::set_value(Complex value)
{
    this->_value = value;
}

void ds::DiffractionSpot::set_weight(double weight)
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

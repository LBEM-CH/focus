/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fom_utilities.hpp"
#include "bessel_functions.hpp"

double volume::utilities::fom_utilities::FomToXarg(double fom)
{
    namespace fm = volume::utilities::fom_utilities;
    double xarg = 0.0;
    
    fom = fom * 100;
    
    //Set a maximum FOM value
    if(fom > 99.0800) fom = 99.0800;
    
    //Set a minimum XARG value
    if(fom < 1.045) return xarg;
    
    int fom_position = 100;
    double fom_in = fm::LOOKUP_FOM_TO_XARG[fom_position][0];
    
    //get the correct FOM position
    while (fom_in > fom)
    {
        fom_in = fm::LOOKUP_FOM_TO_XARG[fom_position][0];
        fom_position--;
    }
    
    //Linear interpolation
    xarg =  (fm::LOOKUP_FOM_TO_XARG[fom_position][1]) 
            + (fm::LOOKUP_FOM_TO_XARG[fom_position+1][1]-fm::LOOKUP_FOM_TO_XARG[fom_position][1]) 
            * ((fom-fm::LOOKUP_FOM_TO_XARG[fom_position][0])/(fm::LOOKUP_FOM_TO_XARG[fom_position+1][0]-fm::LOOKUP_FOM_TO_XARG[fom_position][0]));
    
    return xarg;

}

double volume::utilities::fom_utilities::XargToFom(double xarg)
{
    //Calculate the Bessel function I0(x)
    double i0 = volume::utilities::bessel_functions::i0(xarg);
    
    //Calculate the Bessel function I1(x)
    double i1 = volume::utilities::bessel_functions::i1(xarg);
    
    return i1/i0;
}

double volume::utilities::fom_utilities::AverageFOMs(const std::list<double> foms)
{
    //Get the sum of XARGS from weights
    double xarg_sum = 0.0;
    for(std::list<double>::const_iterator itr = foms.begin(); itr != foms.end(); ++itr)
    {
        double xarg = volume::utilities::fom_utilities::FomToXarg((*itr));
        xarg_sum += xarg;
    }
    
    //Set the upper limit of XARG sum
    if(xarg_sum > 54) xarg_sum = 54;
    
    //Get the averaged weight
    double avg_weight = volume::utilities::fom_utilities::XargToFom(xarg_sum);
    
    return avg_weight;
}
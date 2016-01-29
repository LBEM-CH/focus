/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_utilities.hpp"
#include "fom_utilities.hpp"
#include "2dx_volume_processing/lib/data_structures/reflection_data.hpp"
#include "angle_utilities.hpp"


double tdx::utilities::fourier_utilities::get_resolution
    (const tdx::data::MillerIndex& index, double gamma, double a, double b, double c)
{
    
    if(a==0 || b==0 || c==0 || gamma==0)
    {
        std::cerr << "ERROR! Encountered 0 in cell dimensions while calculating resolution.";
        return 0;
    }
     
    //std::cerr << "::GetResolution: a,b,c,gamma = "<<a<<","<<b<<","<<c<<",  "<<gamma<<"\n";
    //Return infinite resolution if all three h,k,l are 0
    if(index.h() == 0 && index.k() == 0 && index.l() == 0)
    {
        return 100000.0;
    }
    
    //Calculate the reciprocal lattice
    // gamma already in radians
    double astar = 1.0 / ( a * sin(gamma));
    double bstar = 1.0 / ( b * sin(gamma));
    double cstar = 1.0 / c;

    double recgamma = M_PI - gamma;
    
    double dstar_sq = ( pow(index.h()*astar,2) 
                      + 2*index.h()*index.k()*astar*bstar*cos(recgamma)
                      + pow(index.k()*bstar,2)
                      + pow(index.l()*cstar,2) );
    

    double final_resolution = 1/sqrt(dstar_sq);
    
    //Sanity Check
    //if(final_resolution < 1.0 || final_resolution > 100000)
    //{
    //    std::cerr << "WARNING: Resolution of spot (" << index.h() << ", " << index.k() << ", " << index.l() << "gamma= " << gamma <<" ) = " << final_resolution << " exceeds limits\n";
    //}
    
    return final_resolution;
}

void tdx::utilities::fourier_utilities::average_peaks(const tdx::data::MillerToPeakMultiMap& peak_multimap, tdx::data::MillerToPeakMap& peak_map)
{
    peak_map.clear();
    bool initialized = false;
    
    tdx::data::MillerIndex current_index;
    tdx::data::PeakList current_spots;
    
    for(tdx::data::MillerToPeakMultiMap::const_iterator spot_itr=peak_multimap.begin(); spot_itr!=peak_multimap.end(); ++spot_itr)
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
            tdx::data::PeakData avg_spot;
            avg_spot.from_peak_list(current_spots);
            peak_map.insert(tdx::data::MillerToPeakPair(current_index, avg_spot));
            current_spots.clear();
        }
        
        //Accumulate the spots in a list
        current_spots.push_back((*spot_itr).second);
        current_index = (*spot_itr).first;
        
    }
    
    //insert the final reflection
    tdx::data::PeakData avg_spot;
    avg_spot.from_peak_list(current_spots);
    peak_map.insert(tdx::data::MillerToPeakPair(current_index, avg_spot));
}

double tdx::utilities::fourier_utilities::calculate_phase_residual(const tdx::data::PeakList peaks)
{
    tdx::data::Complex sum_values;
    std::list<double> foms;
    double sum_foms = 0.0;
    double phase_residual = 0.0;
    
    for(tdx::data::PeakList::const_iterator peaks_itr = peaks.begin(); 
                peaks_itr != peaks.end(); ++peaks_itr)
    {
        foms.push_back((*peaks_itr).weight());
        sum_values = sum_values + (*peaks_itr).value();
        sum_foms = sum_foms + (*peaks_itr).weight();
    }
    
    //Get the averaged weight
    double avg_weight = tdx::utilities::fom_utilities::AverageFOMs(foms);
    
    //The averaged value
    tdx::data::Complex avg_value = sum_values*(avg_weight/sum_foms);
    
    //Calculate phase residual w.r.t the averaged complex. 
    //The definition comes from Van Hell Ultra Microscopy 21(1978) 95-100 Section 4
    double averaged_phase = avg_value.phase();
    averaged_phase = tdx::utilities::angle_utilities::CorrectRadianPhase(averaged_phase);
    double averaged_amplitude = avg_value.amplitude();
    double numerator_sum = 0.0;
    double denominator_sum = 0.0;
    if(averaged_amplitude > 0.00001)
    {   for(tdx::data::PeakList::const_iterator peaks_itr = peaks.begin(); 
                peaks_itr != peaks.end(); ++peaks_itr)
        {
            double phase = (*peaks_itr).value().phase();
            phase = tdx::utilities::angle_utilities::CorrectRadianPhase(phase);
            double amplitude = (*peaks_itr).value().amplitude();
            denominator_sum += std::abs(averaged_amplitude) * std::abs(amplitude);
            double phase_diff = tdx::utilities::angle_utilities::CorrectRadianPhase(averaged_phase-phase);
            numerator_sum += std::abs(averaged_amplitude) * std::abs(amplitude) *abs(phase_diff);
        }
    
        if(denominator_sum > 0.00001) phase_residual = numerator_sum/denominator_sum;
    }
    return phase_residual*180/M_PI;
    
}

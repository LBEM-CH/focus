/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_symmetrization.hpp"
#include "symmetry_operations.hpp"
#include "../utilities/fourier_utilities.hpp"


void tdx::symmetrization::fourier_symmetrization::symmetrize
        (tdx::data::ReflectionData& fourier_data, 
        const tdx::symmetrization::Symmetry2dx& symmetry)
{
    namespace ds = tdx::data;
    ds::MillerToPeakMultiMap spot_multimap;
    
    for(ds::ReflectionData::const_iterator data_iterator= fourier_data.begin(); data_iterator != fourier_data.end(); ++data_iterator)
    {
        ds::MillerIndex current_index = (*data_iterator).first;
        ds::PeakData current_spot = (*data_iterator).second;
        double current_amp = current_spot.value().amplitude();
        double current_phase = current_spot.value().phase();
        
        //Keep only the spots with a minimum intensity
        if(current_amp > 0.0001)
        {
            //Place the original reflection
            spot_multimap.insert(ds::MillerToPeakPair(current_index, current_spot));
            
            //Loop over all possible symmetry operations
            for(int op_index=0; op_index<30; op_index++)
            {
                tdx::symmetrization::SymmetryOperations operation = 
                        tdx::symmetrization::SymmetryOperations(op_index, symmetry.symmetry_code());
                
                if(!operation.SkipOperation())
                {
                    //get the symmetric miller index
                    int* sym_h = new int(current_index.h());
                    int* sym_k = new int(current_index.k());
                    int* sym_l = new int(current_index.l());
                    
                    operation.SymmetricMillerIndex(sym_h, sym_k, sym_l);
                    ds::MillerIndex sym_index = ds::MillerIndex(*sym_h, *sym_k, *sym_l);
                    
                    //get the new phase
                    double sym_phase = operation.PhaseChange(current_phase, current_index.h(),
                                                                current_index.k(), current_index.l());
                    
                    //For the negative h, get the Friedel spot and change phase
                    if(sym_index.h() < 0)
                    {
                        sym_index = sym_index.FriedelSpot();
                        sym_phase = -1 * sym_phase;
                    }
                    
                    
                    double sym_real = current_amp * cos(sym_phase);
                    double sym_imag = current_amp * sin(sym_phase);
                    ds::PeakData sym_spot(tdx::Complex(sym_real, sym_imag), current_spot.weight());
                    
                    spot_multimap.insert(ds::MillerToPeakPair(sym_index, sym_spot));
                    
                    
                }
                
            }
            
        }
        
    }
    
    ds::MillerToPeakMap averaged_data;
    tdx::utilities::fourier_utilities::average_peaks(spot_multimap, averaged_data);
    fourier_data.reset(averaged_data);
    
}
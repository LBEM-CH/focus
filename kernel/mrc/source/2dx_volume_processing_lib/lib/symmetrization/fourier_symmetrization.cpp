/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "fourier_symmetrization.hpp"
#include "symmetry_operations.hpp"


void volume::symmetrization::fourier_symmetrization::symmetrize
        (volume::data::FourierSpaceData& fourier_data, 
        const volume::symmetrization::Symmetry2dx& symmetry)
{
    namespace ds = volume::data;
    ds::DiffractionSpotMultiMap spot_multimap;
    
    for(ds::FourierSpaceData::const_iterator data_iterator= fourier_data.begin(); data_iterator != fourier_data.end(); ++data_iterator)
    {
        ds::MillerIndex current_index = (*data_iterator).first;
        ds::DiffractionSpot current_spot = (*data_iterator).second;
        double current_amp = current_spot.value().amplitude();
        double current_phase = current_spot.value().phase();
        
        //Keep only the spots with a minimum intensity
        if(current_amp > 0.0001)
        {
            //Place the original reflection
            spot_multimap.insert(ds::MillerIndexDiffSpotPair(current_index, current_spot));
            
            //Loop over all possible symmetry operations
            for(int op_index=0; op_index<30; op_index++)
            {
                volume::symmetrization::SymmetryOperations operation = 
                        volume::symmetrization::SymmetryOperations(op_index, symmetry.symmetry_code());
                
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
                    ds::DiffractionSpot sym_spot(ds::Complex2dx(sym_real, sym_imag), current_spot.weight());
                    
                    spot_multimap.insert(ds::MillerIndexDiffSpotPair(sym_index, sym_spot));
                    
                    
                }
                
            }
            
        }
        
    }
    
    fourier_data = ds::FourierSpaceData(spot_multimap);
    
}
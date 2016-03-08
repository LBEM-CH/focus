/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef REFLECTION_IO_HPP
#define	REFLECTION_IO_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <math.h>

#include "../data_structures/volume_header.hpp"
#include "../data_structures/reflection_data.hpp"

namespace tdx
{
    namespace io
    {
        namespace reflection
        {
            /**
             * Function to read in HKZ/L data. The data is expected to be in the
             * following format:
             * h    k   l/z*   amplitude   phase   sig_amp sig_phase   iq_value
             * @param file_path
             * @param z_scale: the scale which will be used to convert z* data to l (for HKL files z_scale = 1)  
             * @param raw_ccp4 :  Correct for density on top and bottom (as from raw ccp4)?
             * @param[out] Reflections with each h,k,l associated with multiple possible peak values
             */
            void read(std::string file_path, int z_scale, bool raw_ccp4, tdx::data::MillerToPeakMultiMap& peak_multimap);
            
            /**
             * A HKL file writer. Writes the HKL File in the following format:
             * h    k   l   amp phase   fom
             * @param file_path
             * @param data - input data
             * @param for_ccp4 - if the output would be used by CCP4? (will invert the hand and sift the phase)
             */
            void write(const std::string& file_path, const tdx::data::ReflectionData& data, bool for_ccp4 = true);
            
            /**
             * Method to add a spot with raw parameters read from file.
             * @param map
             * @param h_in
             * @param k_in
             * @param z_in
             * @param amp_in
             * @param phase_in
             * @param weight_in
             * @param z_scale
             * @param raw_ccp4
             */
            void add_spot(tdx::data::MillerToPeakMultiMap& map, int h_in, int k_in, double z_in, double amp_in, double phase_in, double weight_in, int z_scale, bool raw_ccp4);
            
            /**
             * Returns the number of columns in a file.
             * Also considers if there are irregular heading lines present 
             * @param file_name : String path of file
             * @param number_columns : Returns the number of columns in this variable
             * @return number of header lines if found
             */
            int number_of_columns(const std::string file_name, int& number_columns);
        }
        
    }
    
}

#endif	/* REFLECTION_IO_HPP */


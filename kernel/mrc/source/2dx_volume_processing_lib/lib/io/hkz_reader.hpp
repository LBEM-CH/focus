/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef HKZ_READER_HPP
#define	HKZ_READER_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <math.h>

#include "../data_structures/volume_header.hpp"
#include "../data_structures/fourier_space_data.hpp"

namespace volume
{
    namespace io
    {
        namespace hkz_reader
        {
            /**
             * Function to read in HKZ data. The data is expected to be in the
             * following format:
             * h    k   l   amplitude   phase   sig_amp sig_phase   iq_value
             * @param file_path
             * @param[in] header
             * @return fourier_data
             */
            volume::data::FourierSpaceData read(std::string file_path,
                    const volume::data::VolumeHeader2dx& header);
            
            
        }
        
    }
    
}

#endif	/* HKZ_READER_HPP */


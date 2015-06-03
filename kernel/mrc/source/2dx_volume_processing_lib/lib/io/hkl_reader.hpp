/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef HKL_READER_HPP
#define	HKL_READER_HPP

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
        namespace hkl_reader
        {
            /**
             * Function to read in HKL data. The data is expected to be in the
             * following format:
             * h    k   l   amplitude   phase   fom
             * @param file_path
             * @param[in] header
             * @return fourier_data
             */
            volume::data::FourierSpaceData read(std::string file_path,
                    const volume::data::VolumeHeader2dx& header, bool raw_ccp4 = true);
            
            
        }
        
    }
    
}

#endif	/* HKL_READER_HPP */


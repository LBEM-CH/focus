/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef HKL_WRITER_HPP
#define	HKL_WRITER_HPP

#include <iostream>
#include <fstream>
#include <iomanip> 
#include <string.h>
#include <math.h>

#include "../data_structures/fourier_space_data.hpp"

namespace volume
{
    namespace io
    {
        namespace hkl_writer
        {
            /**
             * A HKL file writer. Writes the HKL File in the following format:
             * h    k   l   amp phase   fom
             * @param file_path
             * @param data - input data
             */
            void write(const std::string& file_path, const volume::data::FourierSpaceData& data);
            
        }
        
    }
    
}

#endif	/* HKL_WRITER_HPP */


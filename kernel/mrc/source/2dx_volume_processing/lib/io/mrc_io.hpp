/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MRC_IO_HPP
#define	MRC_IO_HPP

#include <iostream>
#include <string.h>
#include <fstream>

#include "../data_structures/volume_header.hpp"
#include "../data_structures/real_space_data.hpp"

namespace volume
{
    namespace io
    {
        namespace mrc
        {   
            /**
             * A function to parse the header parameters from MRC/MAP files
             * @param file_name
             * @param file format - Supported formats: map/mrc
             * @return the header instance
             */
            volume::data::VolumeHeader2dx get_header(const std::string file_name, const std::string format);
            
            /**
             * A function to parse the data from a MRC/MAP file.
             * @param file_name
             * @param nx
             * @param ny
             * @param nz
             * @return an instance of real space data
             */
            volume::data::RealSpaceData get_data(const std::string file_name, const int nx, const int ny, const int nz);
            
            /**
             * Writes a MRC/MAP file with the real space data in MRC data format 2
             * @param file_name
             * @param header
             * @param data
             * @param write format MAP/MRC
             */
            void write_mrc_mode_2(const std::string file_name, const volume::data::VolumeHeader2dx& header,
                       const volume::data::RealSpaceData& data, const std::string format);
            
            
        } // namespace mrc
        
    }//namespace io
    
}// namespace volume

#endif	/* MRC_IO_HPP */


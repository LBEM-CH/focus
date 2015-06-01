/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MAP_WRITER_HPP
#define	MAP_WRITER_HPP

#include <iostream>
#include "../data_structures/volume_header.hpp"
#include "../data_structures/real_space_data.hpp"

namespace volume
{
    namespace io
    {
        namespace map_writer
        {
            /**
             * Writes a MRC file with the real space data
             * @param file_name
             * @param header
             * @param data
             */
            void write_real(const std::string file_name,
                            const volume::data::VolumeHeader2dx& header,
                            const volume::data::RealSpaceData& data);
            
        }
        
    }
    
}

#endif	/* MRC_WRITER_HPP */


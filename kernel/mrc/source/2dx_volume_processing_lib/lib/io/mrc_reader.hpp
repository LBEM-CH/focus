/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MRC_READER_HPP
#define	MRC_READER_HPP

#include <iostream>
#include <string.h>
#include <fstream>

#include "../data_structures/volume_header.hpp"
#include "../data_structures/real_space_data.hpp"

namespace volume_processing_2dx
{
    namespace io
    {
        class MrcReader
        {
        public:
            
            /**
             * Constructor parsing the MRC file with path file_name
             * @param file_name
             */
            MrcReader(std::string file_name);
            
            /**
             * Utility function to convert mrc data to 2dx real data
             * @param header
             * @param data
             * @return 
             */
            bool mrc_to_real(volume_processing_2dx::data_structures::VolumeHeader2dx& header,
                             volume_processing_2dx::data_structures::RealSpaceData& data);
            
        private:
            
            /**
             * A function to parse/read the MRC file and set the data
             * and header values.
             * @param file_name
             * @return True for successful read/ False otherwise
             */
            bool initialize(std::string file_name);
            
            /**
             * A function to parse and set the header parameters from mrc file
             * @param file_name
             * @return True for successful read/ False otherwise
             */
            bool read_header(std::string file_name);
            
            /**
             * A function to parse and set the data from mrc file
             * @param file_name
             * @return True for successful read/ False otherwise
             */
            bool read_data(std::string file_name);
      
                    
            /**
             * MRC header fields
             */
            int nx, ny, nz;
            int nxstart, nystart, nzstart;
            int mode;
            int mx, my, mz;
            float xlen, ylen, zlen;
            float alpha, beta, gamma;
            int mapc, mapr, maps;
            
            /**
             * Data field.
             * Origin at the lower left corner
             * nx fastest changing and nz slowest changing
             */
            float* _data;
            
            
            
        }; // class MrcReader
        
    }//namespace io
    
}// namespace volume_processing_2dx

#endif	/* MRC_READER_HPP */


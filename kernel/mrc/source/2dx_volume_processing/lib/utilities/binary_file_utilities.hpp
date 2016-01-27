/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef BINARY_FILE_UTILITIES_HPP
#define	BINARY_FILE_UTILITIES_HPP

#include <iostream>
#include <fstream>
#include <string>

namespace tdx
{
    namespace utilities
    {
        namespace binary_file_utilities
        {
            /**
             * Returns the size of the input binary file.
             * @param in_file
             * @return size
             */
            long file_size(const std::string& filename);
            
            /**
             * Reads in an integer from the current position of the file.
             * @param in_file
             * @return int
             */
            int read_int(std::istream& in_file);
            
            /**
             * Reads in a float from the current position of the file.
             * @param in_file
             * @return float
             */
            float read_float(std::istream& in_file);

        }
        
    }
    
}

#endif	/* BINARY_FILE_UTILITIES_HPP */


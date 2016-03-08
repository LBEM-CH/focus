/* 
 * File:   File.hpp
 * Author: biyanin
 *
 * Created on March 8, 2016, 10:36 AM
 */

#ifndef BINARY_FILE_HPP
#define	BINARY_FILE_HPP

#include <iostream>
#include <string>
#include <fstream>

#include "file.hpp"

namespace tdx
{
    /**
     * A wrapper class to Read and write the binary files
     */
    class BinaryFile : public File
    {
    public:
        
        /**
         * Creates the object with filename and mode
         * @param file_name
         * @param mode
         */
        BinaryFile(const std::string& file_name, openmode mode = in | out);
        
        /**
         * Reads an string of character size read_size from current position
         * @param read_size (number of characters to be read)
         * @return 
         */
        std::string read_string(const int& read_size);

        /**
         * Reads in an integer from the current position of the file
         * @return int
         */
        int read_int();

        /**
         * Reads in a float from the current position of the file
         * @return float
         */
        float read_float();
        
    private:
        std::string _filename;
    };
}


#endif	/* FILE_HPP */


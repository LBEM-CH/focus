/* 
 * File:   File.hpp
 * Author: biyanin
 *
 * Created on March 8, 2016, 10:36 AM
 */

#ifndef FILE_HPP
#define	FILE_HPP

#include <iostream>
#include <string>
#include <fstream>
#include <sys/stat.h>
#include <sstream>

namespace tdx
{
    /**
     * A wrapper class to Read and write the files
     */
    class File : public std::fstream
    {
    public:
        
        typedef std::ios_base::openmode openmode;
        
        static const openmode in = std::ios::in;
        static const openmode out = std::ios::out;
        
        /**
         * Creates the object with filename and mode
         * @param file_name
         * @param mode
         */
        File(const std::string& file_name, openmode mode = in | out);
        
        /**
         * Name with path of the file under consideration
         * @return (string) file name
         */
        std::string file_name() const;
        
        /**
         * Checks if the file exists on the system
         * @return (bool) if file exists
         */
        bool exists() const;
        
        /**
         * Returns the file extension.
         * Evaluates the letters after the final '.' appearing in the string.
         * If '.' is absent returns ''
         * @return (string) the extension of the file
         */
        std::string extension() const;
        
        /**
         * Returns the size of the input file
         * @return size
         */
        size_t file_size() const;
        
        /**
         * Reads a line from the current position
         * @return (string) line
         */
        std::string read_line();
        
    private:
        std::string _filename;
    };
}


#endif	/* FILE_HPP */


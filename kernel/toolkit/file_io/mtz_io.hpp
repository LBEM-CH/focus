/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef MTZ_IO_HPP
#define	MTZ_IO_HPP

#include <iostream>
#include <string>
#include <fstream>
#include <vector>
#include <cmath>
#include <ctime>

#include "../data_structures/volume_header.hpp"
#include "../data_structures/reflection_data.hpp"

namespace tdx
{
    namespace io
    {
        /**
         * A class to perform MTZ file parsing including reading header and 
         * the data. The data is stored as an instance of class ReflcetionData.
         * Note that the parser is  quite LIMITED for READ/WRTIE. 
         * It can only read a few column type, assumes only one dataset is 
         * present, ignores sort order.
         * 
         * It can only write certain type of columns:
         * With 5 columns: H K L AMP PHASE
         * With 6 columns: H K L AMP PHASE FOM
         * With 7 columns: H K L AMP PHASE FOM SIGF
         */
        class MTZParser
        {
        public:
            
            /**
             * READ MODE constructor with file name to read MTZ file
             * @param file_name (std::string)
             */
            MTZParser(std::string file_name);
            
            /**
             * WRITE MODE constructor with file name and data to WRITING MTZ file
             * @param file_name (std::string)
             * @param data (ReflectionData)
             * @param header (VolumeHeader)
             * @param number_of_columns (int) Number of columns to be written
             * 
             * With 5 columns: H K L AMP PHASE
             * With 6 columns: H K L AMP PHASE FOM
             * With 7 columns: H K L AMP PHASE FOM SIGF
             */
            MTZParser(std::string file_name, tdx::data::ReflectionData data, tdx::data::VolumeHeader header, int number_of_columns=6);
            
            /**
             * Returns the file name used for the output
             * @return (string) file name
             */
            std::string file_name();
            
            /**
             * Returns a printable string containing details of the header
             * @return (string) containing header details
             */
            std::string header_string();
            
            /**
             * Returns the reflection data
             * @return (ReflectionData) stored in the class
             */
            tdx::data::ReflectionData data();
            
            /**
             * Return the volume header from the values stored in the class
             * @return (VolumeHeader) header which can be used to set the volume
             */
            tdx::data::VolumeHeader header();
            
            /**
             * Write the MTZ file from the header and data present
             * It can only write certain type of columns:
             * With 5 columns: H K L AMP PHASE
             * With 6 columns: H K L AMP PHASE FOM
             * With 7 columns: H K L AMP PHASE FOM SIGF
             */
            void write();
            
        private:
            
            /**
             * Reads the data from the header records
             */
            void read_header();
            
            /**
             * Reads the reflection data from the file.
             * Assumes that only one dataset is present in the file
             */
            void read_data();
            
            /**
             * Stores the I/O file name
             */
            std::string _file_name;
            
            /**
             * Number of columns present in the file
             */
            size_t _number_of_columns = 0;
            
            /**
             * Number of reflections present in the file
             */
            size_t _number_of_reflections = 0;
            
            /**
             * Column labels, should be of the size of number of columns
             */
            std::vector<std::string> _column_labels;
            
            /**
             * Column size, should be of the size of number of columns
             */
            std::vector<char> _column_type;
            
            /**
             * Column min values, should be of the size of number of columns
             */
            std::vector<float> _column_min;
            
            /**
             * Column max values, should be of the size of number of columns
             */
            std::vector<float> _column_max;
            
            /**
             * Position where the header starts from!
             */
            size_t _header_position;
            
            /**
             * Title from/to MTZ header
             */
            std::string _title = "";
            
            /**
             * Cell parameters from/to MTZ header
             */
            float _cell[6] = {0, 0, 0, 90, 90, 90};
            
            /**
             * Min and Max resolution values
             */
            float _resolution[2] = {1000, 2};
            
            /**
             * Data stored in the reflections
             */
            tdx::data::ReflectionData _data;
            
        };
        
    }//namespace io
    
}// namespace volume

#endif	/* MTZ_IO_HPP */


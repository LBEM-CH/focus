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

#include "../data_structures/volume_header.hpp"
#include "../data_structures/reflection_data.hpp"

namespace tdx
{
    namespace io
    {
        /**
         * A class to perform utility functions on MTZ files including
         * Reading / writing / printing header information etc.
         */
        class MTZUtils
        {
        public:
            
            /**
             * Constructor with file name
             * @param file_name
             */
            MTZUtils(std::string file_name);
            
            std::string file_name();
            
            std::string header_string();
            
            
        private:
            
            void read_header();
            
            std::vector<std::string> _supported_column_labels = { "H", "I", "K", "L", "FC", "SIGFP", "PHIC", "FOM", "WT"};
            std::vector<char> _supported_column_types = {'H', 'J', 'F', 'P', 'W'};
            
            std::string _file_name;
            
            int _number_of_columns = 0;
            int _number_of_reflections = 0;
            std::vector<std::string> _column_labels;
            std::vector<std::string> _column_type;
            std::vector<int> _column_min;
            std::vector<int> _column_max;
            std::vector<int> _column_id;
            
            int _header_position;
            
            std::string _title = "";
            
            float _cell[6] = {0, 0, 0, 90, 90, 90};
            
            int _sort[5] = {1, 2, 3, 4, 5};
            
            double _resolution[2] = {1000, 2};
            
            std::vector<std::string> _history;
            
        };  
        
        
    }//namespace io
    
}// namespace volume

#endif	/* MTZ_IO_HPP */


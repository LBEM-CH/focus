/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef STRING_UTILITIES_HPP
#define	STRING_UTILITIES_HPP

#include <iostream>
#include <string>
#include <sstream>
#include <vector>

namespace tdx
{
    namespace utilities
    {
        namespace string_utilities
        {
            /**
             * Trims the leading/trailing/extra spaces in the string
             * @param string
             */
            std::string trim(const std::string& str);
            
            /**
             * Splits the string with the given delimiter
             * @param s
             * @param delim
             * @return 
             */
            std::vector<std::string> split(const std::string &s, char delim);
              
        } 
    
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* ANGLE_UTILITIES_HPP */


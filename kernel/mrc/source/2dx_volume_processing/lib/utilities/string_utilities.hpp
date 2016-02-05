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
#include <iomanip>

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
            
            /**
             * Returns a string of precision and width from double
             * @param (double) value
             * @param (int) width
             * @param (int) precision
             * @return (string)
             */
            std::string string_of(double value, int width, int precision);
            
            /**
             * Returns a string of width from int
             * @param (int) value
             * @param (int) width
             * @return (string)
             */
            std::string string_of(int value, int width);
            
            /**
             * Returns a string of width from string
             * @param (string) value
             * @param (int) width
             * @return (string)
             */
            std::string string_of(std::string value, int width);
              
        } 
    
    } // namespace utilities
    
} // namespace volume_processing_2dx

#endif	/* ANGLE_UTILITIES_HPP */


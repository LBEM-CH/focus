/* 
 * File:   string.hpp
 * Author: Nikhil Biyani
 *
 */

#ifndef STRING_HPP
#define	STRING_HPP

#include <iostream>
#include <string>
#include <sstream>
#include <vector>
#include <iomanip>

namespace tdx
{
    /**
     * A class with added functions on string.
     */
    class String
    {
        
    public:
        
        /**
         * Initiate the string from std::string
         * @param (std::string) string
         */
        String(const std::string& string);
        
        /**
         * Returns the std::string
         * @return std::string
         */
        std::string to_std_string() const;
        
        /**
         * Sets the value from std::string
         * @param (std::string) string 
         */
        void from_std_string(const std::string& string);
        
        /**
         * Trims the leading/trailing/extra spaces in the string
         * @param string
         * @return std::string
         */
        static std::string trim(const std::string& str);
        
        /**
         * Trims the leading/trailing/extra spaces in the string
         * @return 
         */
        String trim() const;

        /**
         * Splits the string with the given delimiter
         * @param s
         * @param delim
         * @return 
         */
        static std::vector<std::string> split(const std::string &s, char delim);
        
        /**
         * Splits the string with the given delimiter
         * @param delim
         * @return 
         */
        std::vector<String> split(char delim) const;

        /**
         * Returns a string of precision and width from double
         * @param (double) value
         * @param (int) width
         * @param (int) precision
         * @return (string)
         */
        static std::string string_of(double value, int width, int precision);

        /**
         * Returns a string of width from int
         * @param (int) value
         * @param (int) width
         * @return (string)
         */
        static std::string string_of(int value, int width);

        /**
         * Returns a string of width from string
         * @param (string) value
         * @param (int) width
         * @return (string)
         */
        static std::string string_of(std::string value, int width);
        
    private:
        std::string _value;
    };
}

#endif	/* STRING_HPP */


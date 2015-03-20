/* 
 * File:   SystemUtils.hpp
 * Author: biyanin
 *
 * Created on March 18, 2015, 12:56 PM
 */

#ifndef SYSTEMUTILS_HPP
#define	SYSTEMUTILS_HPP

#include <string.h>

/*
 * Returns the string after last '.' in the input.
 * If not found returns ""
 * Useful for getting the formats of the files
 */
std::string get_extension(std::string fn){
    std::string extension = "";
    
    if(fn.find('.') != std::string::npos){
        extension = fn.substr(fn.find_last_of(".") + 1);
    }
    
    return extension;
}



#endif	/* SYSTEMUTILS_HPP */


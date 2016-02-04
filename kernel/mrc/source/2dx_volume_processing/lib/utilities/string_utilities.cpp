/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include "string_utilities.hpp"


std::string tdx::utilities::string_utilities::trim(const std::string& str)
{
    //Remove extra spaces
    std::string trimmed = "";
    bool spaced = false;

    for(const char& curr : str)
    {
        if(spaced && curr != ' ') spaced = false;
        if(!spaced) trimmed += curr;
        if(curr == ' ') spaced = true;
    }

    //Remove beginning and end white space if exists
    if(trimmed.back() == ' ') trimmed = trimmed.substr(0, trimmed.length()-1);
    if(trimmed[0] == ' ') trimmed = trimmed.substr(1, trimmed.length()-1);

    return trimmed;

}

std::vector<std::string> tdx::utilities::string_utilities::split(const std::string &s, char delim) 
{
    std::vector<std::string> elems;
    std::stringstream ss(s);
    std::string item;
    while (std::getline(ss, item, delim)) 
    {
        elems.push_back(item);
    }
    return elems;
}
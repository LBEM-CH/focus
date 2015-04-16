/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "../include/2dx_volume_processing.h"

int main(int argc, char** argv)
{
    
    if(argc < 2)
    {
        std::cout << "Program Options\n\t<mrc file>\n";
        return(1);
    }
    
    
    std::string mrcFileName = argv[1];
    
    Volume2dx volume;
    volume.read_volume(mrcFileName, "mrc");
    
    
}

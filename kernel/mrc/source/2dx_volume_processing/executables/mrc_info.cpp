/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "../include/2dx_volume_processing.h"

int main(int argc, char* argv[])
{
    
    args::Executable exe("Program to produce the information of mrc/map.", ' ', "1.0" );
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    
    //Add arguments
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(args::templates::MRCIN.getValue());
    
    input.prepare_real();
    input.prepare_fourier();
    std::cout << input.to_string();
    
    return 0;
}

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
    
    args::Executable exe("Program to produce the information of mrc/map. The information includes both header and data.", ' ', "1.0" );
    
    //Select required arguments
    TCLAP::UnlabeledValueArg<std::string> INPUT_VOLUME("input", "Input MRC/MAP file", true, "", "MRC FILE");
    
    //Add arguments
    exe.add(INPUT_VOLUME);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(INPUT_VOLUME.getValue());
    
    input.prepare_real();
    input.prepare_fourier();
    std::cout << input.to_string();
    
    return 0;
}

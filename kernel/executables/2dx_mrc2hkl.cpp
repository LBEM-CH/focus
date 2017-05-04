/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "2dx_toolkit.h"

int main(int argc, char* argv[])
{
    
    args::Executable exe("Program to convert mrc/map files to hkl.", ' ', "1.0" );
    
    //Select required arguments
    TCLAP::UnlabeledValueArg<std::string> INPUT_VOLUME("input", "Input MRC/MAP file", true, "", "MRC FILE");
    TCLAP::UnlabeledValueArg<std::string> OUTPUT_VOLUME("output", "OUTPUT HKL file", true, "", "HKL FILE");
    
    //Add arguments
    exe.add(INPUT_VOLUME);
    exe.add(OUTPUT_VOLUME);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    Volume2DX input;
    input.read_volume(INPUT_VOLUME.getValue());
    
    input.write_volume(OUTPUT_VOLUME.getValue(), "hkl");
    
    return 0;
}

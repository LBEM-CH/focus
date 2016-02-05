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
    
    args::Executable exe("Program to produce the information of mtz. The information includes both header and data.", ' ', "1.0" );
    
    //Select required arguments
    TCLAP::UnlabeledValueArg<std::string> INPUT_VOLUME("input", "Input MTZ file", true, "", "MTZ FILE");
    
    //Add arguments
    exe.add(INPUT_VOLUME);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    tdx::io::MTZUtils mtz(INPUT_VOLUME.getValue());
    std::cout << mtz.header_string();
    std::cout << "\n\n Number of reflections read: " << mtz.data().spots() << "\n";
    std::cout << " Intensity sum: "  << mtz.data().intensity_sum() << "\n\n";
    
    return 0;
}

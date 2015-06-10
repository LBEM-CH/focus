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
    args::Executable exe("A program to generate bead model of the input MAP/MRC.", ' ', "1.0" );
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::BEADS.forceRequired();
    args::templates::THRESHOLD.forceRequired();
    args::templates::MAXRES.forceRequired();
    
    //Add arguments  
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::THRESHOLD);
    exe.add(args::templates::BEADS);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(args::templates::MRCIN.getValue());
    
    Volume2dx bead_model = input.generate_bead_model(args::templates::BEADS.getValue(), args::templates::THRESHOLD.getValue(), args::templates::MAXRES.getValue());
    
    if(args::templates::HKLOUT.getValue() != "") bead_model.write_volume(args::templates::HKLOUT.getValue(), "hkl");
    if(args::templates::MRCOUT.getValue() != "") bead_model.write_volume(args::templates::MRCOUT.getValue());
    
    return 0;
    
}

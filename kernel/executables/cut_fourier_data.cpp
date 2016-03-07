/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>
#include <vector>

#include "../include/2dx_volume_processing.h"

int main(int argc, char* argv[])
{
    
    args::Executable exe("Cuts the specified type of data from Fourier space of the input map/mrc ", ' ', "1.0" );
    
    TCLAP::ValueArg<int> XY_PLANE("", "xyplane", "Cut the specified XY plane from Fourier space", false, 0, "INT");
    TCLAP::ValueArg<double> CONE("", "cone", "Cut the cone with specified angle (in degrees) from Fourier space", false, 30.0, "FLOAT");
    
    
    TCLAP::ValueArg<std::string> CUT_OUT("", "cutout", "Base name for writing output files (mrc/hkl) for what's been cut", false, "", "STRING");
    TCLAP::ValueArg<std::string> VOLUME_OUT("", "volout", "Base name for writing output files (mrc/hkl) for volume with cut", false, "","STRING");
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    
    //Parse the arguments
    exe.add(CUT_OUT);
    exe.add(VOLUME_OUT);
    exe.add(args::templates::MRCIN);
    
    std::vector<args::Arg*> optionArgs = {&XY_PLANE, &CONE};
    exe.xorAdd(optionArgs);
    
    exe.parse(argc, argv); 
    
    //Check for arguments
    if(!(XY_PLANE.isSet()) && !(CONE.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify what needs to be cut from Fourier space with --xyplane or --cone\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    //Prepare the input
    Volume2DX input;
    input.read_volume(args::templates::MRCIN.getValue());
    
    Volume2DX cutout;
    Volume2DX after_cut;
    
    if(XY_PLANE.isSet()) input.cut_xy_plane(cutout, after_cut, XY_PLANE.getValue());
    if(CONE.isSet()) input.cut_cone(cutout, after_cut, CONE.getValue());
    
    //Write output
    if(CUT_OUT.isSet())
    {
        std::string out_name = CUT_OUT.getValue();
        cutout.write_volume(out_name + ".hkl", "hkl");
        cutout.write_volume(out_name + ".mrc", "mrc");
    }
    
    if(VOLUME_OUT.isSet())
    {
        std::string out_name = VOLUME_OUT.getValue();
        after_cut.write_volume(out_name + ".hkl", "hkl");
        after_cut.write_volume(out_name + ".mrc", "mrc");
    }
    
    return 0;
}

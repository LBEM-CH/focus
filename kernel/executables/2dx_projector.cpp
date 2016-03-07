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
    
    args::Executable exe("Program to get 2D projection of mrc/map.", ' ', "1.0" );
    
    //Add new arguments
    TCLAP::ValueArg<char> PROJECT_AXIS("", "axis", "Axis of projection (x/y/z)", true, 'z',"CHAR");
    TCLAP::SwitchArg AVERAGE("", "average", "Get a real space average along the axis provided (instead of projection)", false);
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::MRCOUT.forceRequired();
    
    //Add arguments
    exe.add(args::templates::MRCOUT);
    exe.add(AVERAGE);
    exe.add(PROJECT_AXIS);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    Volume2DX input;
    input.read_volume(args::templates::MRCIN.getValue());
    
    if(! AVERAGE.getValue())
    {
        Volume2DX projection = input.projection2D(PROJECT_AXIS.getValue());
        projection.write_volume(args::templates::MRCOUT.getValue());
    }
    else
    {
        Volume2DX average = input.average2D(PROJECT_AXIS.getValue());
        average.write_volume(args::templates::MRCOUT.getValue());
    }
    
    return 0;
}

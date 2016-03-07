/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "2dx_toolkit.h"

/*
 * A Universal volume processor. 
 */
int main(int argc, char* argv[]) 
{
    args::Executable exe("A universal 2D crystallography stack processor.", ' ', "1.0" );
    
    //Custom arguments
    TCLAP::ValueArg<int> START("", "start", "Starting frame number for consideration (starting from 0,1,2,3..)", false, -1, "INT");
    TCLAP::ValueArg<int> END("", "end", "Last frame number for consideration (starting from 0,1,2,3..) if end=start only one frame will be considered", false, -1, "INT");
    
    args::templates::MRCIN.forceRequired();
    args::templates::MRCOUT.forceRequired();
    
    //Add arguments
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::ZERO_PHASES);
    exe.add(END);
    exe.add(START);
    exe.add(args::templates::MRCIN);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    tdx::data::VolumeStack input_stack(args::templates::MRCIN.getValue(), START.getValue(), END.getValue());
    
    std::cout << "\nNumber of frames being considered in the movie: " << input_stack.number_of_frames() << "\n\n";
    
    if(args::templates::ZERO_PHASES.getValue())
    {
        tdx::data::VolumeHeader header = input_stack.header();
        std::cout << "Setting all phases to zero..\n";
        for(int frame=0; frame<input_stack.number_of_frames(); frame++)
        {
            std::cout << "Processing frame: " << frame << " ..\n";
            Volume2DX frame_vol(header.rows(), header.columns(), 1);
            frame_vol.set_real(input_stack.get_frame(frame));
            frame_vol.zero_phases();
            input_stack.set_frame(frame, frame_vol.get_real());
        }
    }
    
    if(args::templates::MRCOUT.isSet())
    {
        Volume2DX output = input_stack.to_3D_volume();
        output.write_volume(args::templates::MRCOUT.getValue());;
    }
    
    return 0;
}


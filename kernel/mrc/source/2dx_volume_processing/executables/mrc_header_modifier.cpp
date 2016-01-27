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
    
    args::Executable exe("Program to change header info in mrc/map files. !!USE AT YOUR OWN RISK!!", ' ', "1.0" );
    
    TCLAP::ValueArg<double> CELLX("", "cellx", "Real cell length a", false, 100,"FLOAT");
    TCLAP::ValueArg<double> CELLY("", "celly", "Real cell length b", false, 100,"FLOAT");
    TCLAP::ValueArg<double> CELLZ("", "cellz", "Real cell length b", false, 100,"FLOAT");
    TCLAP::ValueArg<double> GAMMA("", "gamma", "Cell angle gamma", false, 90.0,"FLOAT");
    TCLAP::ValueArg<int> XSTART("", "xstart", "Starting index of x", false, 0,"INT");
    TCLAP::ValueArg<int> YSTART("", "ystart", "Starting index of y", false, 0,"INT");
    TCLAP::ValueArg<int> ZSTART("", "zstart", "Starting index of z", false, 0,"INT");
    
    //Select required arguments
    args::templates::MRCIN.forceRequired();
    args::templates::MRCOUT.forceRequired();
    
    //Add arguments
    exe.add(args::templates::MRCIN);
    exe.add(args::templates::MRCOUT);
    exe.add(CELLX);
    exe.add(CELLY);
    exe.add(CELLZ);
    exe.add(GAMMA);
    exe.add(XSTART);
    exe.add(YSTART);
    exe.add(ZSTART);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    std::string mrcout = args::templates::MRCOUT.getValue();
    
    //Prepare the input
    Volume2dx input;
    input.read_volume(args::templates::MRCIN.getValue());
    tdx::data::VolumeHeader header = input.header();
    if(CELLX.isSet()) header.set_xlen(CELLX.getValue());
    if(CELLY.isSet()) header.set_ylen(CELLY.getValue());
    if(CELLZ.isSet()) header.set_zlen(CELLZ.getValue());
    if(GAMMA.isSet()) header.set_gamma(GAMMA.getValue());
    if(XSTART.isSet()) header.set_nxstart(XSTART.getValue());
    if(YSTART.isSet()) header.set_nystart(YSTART.getValue());
    if(ZSTART.isSet()) header.set_nzstart(ZSTART.getValue());
    
    Volume2dx output(header);
    output.set_real(input.get_real());
    
    if(mrcout != "") output.write_volume(mrcout);
    
    return 0;
}

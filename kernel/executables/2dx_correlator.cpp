/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <iterator>
#include <string>
#include <vector>

#include "../include/2dx_volume_processing.h"

/*
 * Calculates the correlation measures between two volumes such as:
 * Fourier shell correlation, Fourier conic correlation
 */
int main(int argc, char* argv[]) 
{
    args::Executable exe("A script to calculate the correlation between two volumes such as FSC, FCC", ' ', "1.0" );
    
    //Custom arguments
    TCLAP::ValueArg<std::string> VOL1("", "vol1", "Input MRC/MAP volume 1", true, "","FILE");
    TCLAP::ValueArg<std::string> VOL2("", "vol2", "Input MRC/MAP volume 2", true, "","FILE");
    TCLAP::ValueArg<std::string> FSC("", "fsc", "Output file containing the FSC plot", false, "","FILE");
    TCLAP::ValueArg<std::string> FCC("", "fcc", "Output file containing the FCC plot", false, "","FILE");
    TCLAP::ValueArg<std::string> CRC("", "crc", "Output file containing the CRC plot", false, "","FILE");
    TCLAP::ValueArg<std::string> FCMC("", "fcmc", "Output file containing the FCMC plot", false, "","FILE");
    TCLAP::ValueArg<int> BINS("", "bins", "Number of bins to be used", false, 50, "INT");
    
    //Add arguments from templates
    exe.add(FCMC);
    exe.add(CRC);
    exe.add(FCC);
    exe.add(FSC);
    exe.add(BINS);
    exe.add(args::templates::MAXRES);
    exe.add(VOL2);
    exe.add(VOL1);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    std::string infile_volume1 = VOL1.getValue();
    std::string infile_volume2 = VOL2.getValue();
    
    double resolution_max = 2.0;
    if(args::templates::MAXRES.isSet()) resolution_max = args::templates::MAXRES.getValue();
    
    //Read the data 
    Volume2DX volume1;
    volume1.read_volume(infile_volume1);
    
    Volume2DX volume2;
    volume2.read_volume(infile_volume2);
    
    volume1.low_pass(resolution_max);
    volume2.low_pass(resolution_max);
    
    //Bring the volumes to same scale.
    volume1.rescale_to_max_amplitude(1000);
    volume2.rescale_energy(volume1.get_fourier().intensity_sum());
    
    //Calculate binned FSC
    if(FSC.isSet())
    {
        tdx::data::BinnedData binnedFSC = volume1.fourier_shell_correlation(volume2, 0, 1/resolution_max, BINS.getValue());
        
        //Write out binned data
        binnedFSC.write_sum(FSC.getValue());
        std::cout << "\n\n--------------------------\n";
        std::cout << "Averaged binned FSC:\n";
        std::cout << "--------------------------\n";
        std::cout << binnedFSC.plot_sum();
    }
    
    //Calculate binned FCC
    if(FCC.isSet())
    {
        tdx::data::BinnedData binnedFCC = volume1.fourier_conic_correlation(volume2, 0, 90, BINS.getValue());
        
        //Write out binned data
        binnedFCC.write_sum(FCC.getValue());
        std::cout << "\n\n--------------------------\n";
        std::cout << "Averaged binned FCC:\n";
        std::cout << "--------------------------\n";
        std::cout << binnedFCC.plot_sum();
    }
    
    //Calculate CRC
    if(CRC.isSet())
    {
        tdx::data::MeshBinnedData binnedCRC = volume1.cylindrical_ring_correlation(volume2, BINS.getValue());
        binnedCRC.write_sum(CRC.getValue());
    }
    
    //Calculate binned FCMC
    if(FCC.isSet())
    {
        tdx::data::MeshBinnedData binnedFCMC = volume1.fourier_conic_mesh_correlation(volume2);
        
        //Write out binned data
        binnedFCMC.write_sum(FCMC.getValue());
    }
    
    return 0;
}


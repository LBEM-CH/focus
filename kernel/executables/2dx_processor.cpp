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
    args::Executable exe("A universal 2D crystallography volume processor.", ' ', "1.0" );
    
    //Add arguments
    exe.add(args::templates::MTZOUT);
    exe.add(args::templates::MRCOUT);
    exe.add(args::templates::HKLOUT);
    exe.add(args::templates::NORMALIZE_GREY);
    exe.add(args::templates::SPREAD_FOURIER);
    exe.add(args::templates::FULL_FOURIER);
    exe.add(args::templates::SUBSAMPLE);
    exe.add(args::templates::BFACTOR);
    exe.add(args::templates::PSF);
    exe.add(args::templates::ZERO_PHASES);
    exe.add(args::templates::SHIFTZ);
    exe.add(args::templates::SHIFTY);
    exe.add(args::templates::SHIFTX);
    exe.add(args::templates::INVERTZ);
    exe.add(args::templates::INVERTY);
    exe.add(args::templates::INVERTX);
    exe.add(args::templates::INVERTED);
    exe.add(args::templates::EXTENDED);
    exe.add(args::templates::THRESHOLD);
    exe.add(args::templates::MAXAMP);
    exe.add(args::templates::MAXRES);
    exe.add(args::templates::SYMMETRY);
    exe.add(args::templates::GAMMA);
    exe.add(args::templates::NZ);
    exe.add(args::templates::NY);
    exe.add(args::templates::NX);
    
    
    std::vector<args::Arg*> infileArgs = {&args::templates::HKZIN, &args::templates::HKLIN, &args::templates::MRCIN, &args::templates::MTZIN};
    exe.xorAdd(infileArgs);
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Get and check the variables
    std::string infile;
    std::string informat;
    if(args::templates::MRCIN.isSet())
    {
        infile = args::templates::MRCIN.getValue();
        tdx::File file(infile);
        informat = file.extension();
    }
    else if(args::templates::HKLIN.isSet())
    {
        infile = args::templates::HKLIN.getValue();
        informat = "hkl";
    }
    else if(args::templates::HKZIN.isSet())
    {
        infile = args::templates::HKZIN.getValue();
        informat = "hkz";
    }
    else if(args::templates::MTZIN.isSet())
    {
        infile = args::templates::MTZIN.getValue();
        informat = "mtz";
    }
    
    if(!(args::templates::HKLOUT.isSet()) && !(args::templates::MRCOUT.isSet()) && !(args::templates::MTZOUT.isSet()))
    {
        std::cerr << "\n\nERROR: Please specify at least one output with hklout or mrcout or mtzout!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    if((args::templates::HKLIN.isSet() || args::templates::HKZIN.isSet()) && (!(args::templates::NX.isSet()) || !(args::templates::NY.isSet()) || !(args::templates::NZ.isSet())))
    {
        std::cerr << "\n\nERROR: Please specify nx, ny, nz with the option hklin/hkzin!\n";
        std::cerr << "\nFor full details type:\n\t" << exe.getProgramName() << " --help \n\n\n";
        exit(1);
    }
    
    //Prepare the input
    Volume2DX input((int)args::templates::NX.getValue(), (int)args::templates::NY.getValue(), (int)args::templates::NZ.getValue());
    if(args::templates::GAMMA.isSet()) input.set_gamma_degrees(args::templates::GAMMA.getValue());
    
    input.read_volume(infile, informat);
    
    //Would need to overwrite gamma from mrc header
    if(args::templates::GAMMA.isSet()) input.set_gamma_degrees(args::templates::GAMMA.getValue());
    
    if(args::templates::SPREAD_FOURIER.getValue()) input = input.spread_fourier_data();
    
    if(args::templates::MAXRES.isSet()) input.low_pass(args::templates::MAXRES.getValue());
    if(args::templates::SYMMETRY.isSet())
    {
        input.set_symmetry(args::templates::SYMMETRY.getValue());
        input.symmetrize();
    }
    
    if(args::templates::MAXAMP.isSet()) input.rescale_to_max_amplitude(args::templates::MAXAMP.getValue());
    if(args::templates::THRESHOLD.isSet()) input.apply_density_threshold(args::templates::THRESHOLD.getValue());
    
    if(args::templates::BFACTOR.isSet()) input = input.apply_bfactor(args::templates::BFACTOR.getValue());
  
    if(args::templates::EXTENDED.isSet()) input = input.extended_volume(args::templates::EXTENDED.getValue()-1,args::templates::EXTENDED.getValue()-1,0);
    
    if(args::templates::INVERTED.getValue()) input.invert_hand(0);
    if(args::templates::INVERTX.getValue()) input.invert_hand(1);
    if(args::templates::INVERTY.getValue()) input.invert_hand(2);
    if(args::templates::INVERTZ.getValue()) input.invert_hand(3);
    
    
    if(args::templates::SHIFTX.isSet() || args::templates::SHIFTY.isSet() ||  args::templates::SHIFTZ.isSet()) 
    {
        double x_shift = args::templates::SHIFTX.getValue();
        double y_shift = args::templates::SHIFTY.getValue();
        double z_shift = args::templates::SHIFTZ.getValue();
        
        std::cout << "Shifting volume by: " << x_shift << " " << y_shift << " " << z_shift << "\n";
        
        input.shift_volume(x_shift, y_shift, z_shift);
        
    }
    
    if(args::templates::ZERO_PHASES.getValue())
    {
        std::cout << "Setting all phases to zero..\n";
        input = input.zero_phases();
    }
    
    if(args::templates::PSF.getValue())
    {
        std::cout << "Creating PSF\n";
        input = input.zero_phases();
        input.centerize_density_along_xyz();
        input.grey_scale_densities();
    }
    
    if(args::templates::SUBSAMPLE.isSet()) input = input.subsample(args::templates::SUBSAMPLE.getValue());
    
    if(args::templates::FULL_FOURIER.getValue()) input.extend_to_full_fourier();
    
    if(args::templates::NORMALIZE_GREY.getValue()) input.grey_scale_densities();
    
    if(args::templates::HKLOUT.isSet()) input.write_volume(args::templates::HKLOUT.getValue(), "hkl");
    if(args::templates::MRCOUT.isSet()) input.write_volume(args::templates::MRCOUT.getValue());
    if(args::templates::MTZOUT.isSet())  input.write_volume(args::templates::MTZOUT.getValue(), "mtz");
    
    return 0;
}


/* 
 * File:   main.cpp
 * Author: biyanin
 *
 * Created on January 13, 2015, 2:12 PM
 */

#include <cstdlib>
#include <iostream>
#include <fstream>
#include <string>

#include "../../include/VolumeFourier.hpp"
#include "../../include/VolumeReal.hpp"
#include "../../include/VolumeHeader.hpp"
#include "../../include/FourierTransform.hpp"

/*
 * 
 */
int main(int argc, char** argv) {
    
    /*
     * Read in the inputs:
     * <prog_name> hkz_file symmetry nx ny nz apix max_resolution
    */
    
    if (argc < 8) {
        std::cout << "Program Options\n\t<hkzfile> <symmetry> <nx> <ny> <nz> <gamma> <apix> <max_resolution>\n";
        std::cin.get();
        exit(0);
    }
    
    std::string hkzFileName(argv[1]);
    
    std::string symmetry = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    double gamma = std::atof(argv[6]);
    
    double apix = std::atof(argv[7]);
    double max_resolution = std::atof(argv[8]);
    
    //Set the header
    VolumeHeader header;
    header.nx = nx;
    header.ny = ny;
    header.nz = nz;
    header.symmetry = symmetry;
    header.apix = apix;
    header.max_resolution = max_resolution;
    
    
    //Assign the volume
    VolumeFourier inputVol = VolumeFourier(header);
    std::cout << "::Reading in the hkz file from " << argv[1] << "\n";
    inputVol.read_volume(hkzFileName);
    std::cout << ":Created hkl volume\n";
    
    std::cout << ":Symmetrizing with " << symmetry << "\n";
    inputVol.symmetrize();
    inputVol.write_volume("output.hkl");
    
    /*
    FourierTransform transform;
    
    std::cout << "::Doing inverse Fourier transform \n";
    VolumeReal realVol = transform.fft(inputVol);
    
    std::cout << "::Doing Fourier transform \n";
    VolumeFourier fouVol = transform.fft(realVol);
    */
    
    std::cout << "::Done processing hkz file \n";
    

}


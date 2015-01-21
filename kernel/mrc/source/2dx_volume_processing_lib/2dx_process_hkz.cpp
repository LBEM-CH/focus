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

#include "VolumeHKL.hpp"
#include "NumericalUtils.hpp"

/*
 * 
 */
int main(int argc, char** argv) {
    
    /*
     * Read in the inputs:
     * <prog_name> hkz_file symmetry nx ny nz apix max_resolution
    */
    if (argc < 7) {
        std::cout << "Program Options\n\t<hkzfile> <symmetry> <nx> <ny> <nz> <apix> <max_resolution>\n";
        std::cin.get();
        exit(0);
    }
    
    std::ifstream hkzFile(argv[1]);
    
    std::string symmetry = argv[2];
    
    int nx = std::atoi(argv[3]);
    int ny = std::atoi(argv[4]);
    int nz = std::atoi(argv[5]);
    
    double apix = std::atof(argv[6]);
    double max_resolution = std::atof(argv[7]);
    
    VolumeHKL inputVol = VolumeHKL(nx, ny, nz, symmetry, apix, max_resolution, 0.001);
    inputVol.addHKZData(hkzFile);
    
    hkzFile.close();
    
    inputVol.symmetrize();
    inputVol.writeHKL("output.hkl");

}


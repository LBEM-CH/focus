/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iostream>
#include <string.h>

#include "2dx_toolkit.h"

template<typename ValueType_>
std::vector<ValueType_>
matrix_multiply(const std::vector<ValueType_>& mat1, const std::vector<ValueType_>& mat2, int r1, int c1, int c2){

    std::vector<ValueType_> mult(r1*c2, ValueType_());

    for(int i = 0; i < r1; ++i) {
        for(int j = 0; j < c2; ++j) {
            for(int k = 0; k < c1; ++k) {
                mult[i+j*r1] += mat1[i+k*r1] * mat2[k+j*c1];
            }
        }
    }

    return mult;

}

void add_index(const tdx::data::MillerIndex& index_in, const tdx::Complex& value, 
        tdx::data::MillerToPeakMultiMap& current_refs, double psi, double theta, double phi) {

    // Values for transformation
    double cpsi = cos(psi);
    double cthe = cos(theta);
    double cphi = cos(phi);
    double spsi = sin(psi);
    double sthe = sin(theta);
    double sphi = sin(phi);

    //TRANFORMATION MATRICES
    std::vector<double> A = {cpsi, -1*spsi, 0, spsi, cpsi, 0, 0, 0, 1};
    std::vector<double> B = {cthe, 0, sthe, 0, 1, 0, -1*sthe, 0, cthe};
    std::vector<double> C = {cphi, -1*sphi, 0, sphi, cphi, 0, 0, 0, 1};
    std::vector<double> in = {index_in.h()*1.0, index_in.k()*1.0, index_in.l()*1.0};
    
    std::vector<double> res = matrix_multiply(in, matrix_multiply(A, matrix_multiply(B, C, 3, 3, 3), 3, 3, 3), 1, 3, 3);
    
    int h_min = floor(res.at(0));
    int k_min = floor(res.at(1));
    int l_min = floor(res.at(2));
    
    int h_max = ceil(res.at(0));
    int k_max = ceil(res.at(1));
    int l_max = ceil(res.at(2));
    
    for(int h = h_min; h <= h_max; ++h) {
        for(int k = k_min; k<= k_max; ++k) {
            for(int l = l_min; l<-l_max; ++l) {
                
                double dist = sqrt((res[0]-h)*(res[0]-h) + (res[1]-k)*(res[1]-k) + (res[2]-l)*(res[2]-l));
                double sinc = 1;
                if(dist != 0) sinc = sin(M_PI*dist)/(M_PI*dist);
                auto amp_in = value.amplitude();
                auto phase_in = value.phase();
                
                tdx::data::MillerIndex new_idx(h, k, l);

                if(new_idx.h() < 0) {
                    new_idx = new_idx.FriedelSpot();
                    phase_in *= -1;
                }

                double real_in = amp_in*sinc*cos(phase_in);
                double imag_in = amp_in*sinc*sin(phase_in);

                tdx::Complex complex_in(real_in, imag_in);
                tdx::data::PeakData value_out(complex_in, 1.0);

                current_refs.insert(tdx::data::MillerToPeakPair(new_idx, value_out));
            }
        }
    }
}

int main(int argc, char* argv[])
{
    
    args::Executable exe("Program to backproject particles to 3D volume.", ' ', "1.0" );
    
    //Select required arguments
    TCLAP::ValueArg<std::string> INPUT_VOLUME("", "input", "Input MRC stack file", true, "", "MRC FILE");
    TCLAP::ValueArg<std::string> INPUT_PAR("", "par", "Input PAR file", true, "", "PAR FILE");
    TCLAP::ValueArg<std::string> OUTPUT_VOLUME("", "output", "OUTPUT Volume file", true, "", "MRC FILE");
    TCLAP::ValueArg<int> NUM_PARTICLES("", "particles", "Number of particles to consider", false, 0, "INT");
    TCLAP::ValueArg<double> APIX("", "apix", "Pixel size (A/pixel)", false, 1.0, "FLOAT");
    TCLAP::ValueArg<double> RES("", "cut_off", "Resolution cut-off (in A)", false, 1.0, "FLOAT");
    
    INPUT_VOLUME.forceRequired();
    INPUT_PAR.forceRequired();
    OUTPUT_VOLUME.forceRequired();
    
    //Add arguments
    exe.add(OUTPUT_VOLUME);
    exe.add(RES);
    exe.add(NUM_PARTICLES);
    exe.add(APIX);
    exe.add(INPUT_PAR);
    exe.add(INPUT_VOLUME);
    
    
    //Parse the arguments
    exe.parse(argc, argv);
    
    //Prepare the input
    Volume2DX input;
    std::cout << "Reading the particles... \n";
    input.read_volume(INPUT_VOLUME.getValue());
    
    //Get number of particles to process
    int number_particles = input.nz();
    if(NUM_PARTICLES.isSet() && NUM_PARTICLES.getValue() < number_particles) {
        number_particles = NUM_PARTICLES.getValue();
    } 
    std::cout << number_particles << " particles would be processed..\n";
    
    //Get apix
    double apix = APIX.getValue();
    std::cout << "Pixel size is set to: " << apix << " A/px \n";
    
    //Read the par file
    std::vector<std::vector<double> > parameters(number_particles, std::vector<double>(5, 0.0));
    tdx::File inFile (INPUT_PAR.getValue(), tdx::File::in);
    if (!inFile.exists()){
        std::cerr << "File not found: " << INPUT_PAR.getValue() << std::endl;
        exit(1);
    }
    while(!inFile.eof()) {
        std::string line = inFile.read_line();
        if(line.length()==0) continue;
        if(line[0] == 'C' || line[0] == 'c') continue;
        std::vector<std::string> values = tdx::String::split(tdx::String::trim(line), ' ');
        if(values.size() < 6) {
            //std::cerr << "Omitting " << line << "\n";
            continue;
        }
        int particle = std::stoi(values[0])-1;
        if(particle < number_particles) {
            parameters[particle] = {std::stod(values[1]), std::stod(values[2]), std::stod(values[3]), std::stod(values[4]), std::stod(values[5])};
        }
    }
    
    tdx::data::MillerToPeakMultiMap all_reflections;
    for(int i=0; i< number_particles; ++i) {
        std::cout << "Processing particle: " << i+1 <<std::endl;
        Volume2DX slice = input.get_slice(i);
        double psi = parameters[i][0] * M_PI / 180;
        double theta = parameters[i][1] * M_PI / 180;
        double phi = parameters[i][2] * M_PI / 180;
        double x_shift = parameters[i][3] / apix;
        double y_shift = parameters[i][4] / apix;
        
        std::cout << psi << " " << theta << " " << phi << " " << x_shift << " " << y_shift << "\n";
        
        //Shift
        slice.shift_volume(x_shift, y_shift, 0.0);
        
        //Transform
        auto data = slice.get_fourier();
        for(const auto& itr : data) {
            add_index(itr.first, itr.second.value(), all_reflections, psi, theta, phi);
        }
        
    }
    
    std::cout << "Total number of reflections found in 3D Fourier space: " << all_reflections.size() << std::endl;
    tdx::data::MillerToPeakMap averaged_data;
    tdx::utilities::fourier_utilities::average_peaks(all_reflections, averaged_data);
    tdx::data::ReflectionData fourier_data;
    fourier_data.reset(averaged_data);
    
    Volume2DX output(input.nx(), input.ny(), std::max(input.nx(), input.ny()));
    output.set_fourier(fourier_data);
    output.low_pass(RES.getValue()/apix);
    
    output.write_volume(OUTPUT_VOLUME.getValue(), "mrc");
    
    return 0;
}

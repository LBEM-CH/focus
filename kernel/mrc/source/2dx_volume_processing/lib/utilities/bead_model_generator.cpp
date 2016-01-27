/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#include <iomanip> 

#include "../data_structures/real_space_data.hpp"

#include "bead_model_generator.hpp"
#include "angle_utilities.hpp"
#include "density_generator.hpp"

tdx::utilities::BeadModelGenerator::BeadModelGenerator(int number_of_beads, double density_threshold, double noise_level, double max_resolution)
{
    this->number_of_beads = number_of_beads;
    this->density_threshold = density_threshold;
    this->noise_level = noise_level;
    this->max_resolution = max_resolution;
}

void tdx::utilities::BeadModelGenerator::generate_bead_model_coordinates(tdx::data::Volume volume, std::string output_pdb_file)
{
    
    //Prepare the output stream
    std::ofstream out_file_stream;
    
    tdx::data::RealSpaceData real = volume.get_real();
    
    out_file_stream.open(output_pdb_file);
    write_pdb_header(out_file_stream, volume.xlen(), volume.ylen(), volume.zlen(),
                      90, 90, tdx::utilities::angle_utilities::RadianToDegree(volume.gamma()), volume.symmetry());
    
    int x, y, z = 0;
    for ( int bead = 0; bead < number_of_beads; bead++ ) 
    {
        do 
        {
            x = rand() % volume.nx();
            y = rand() % volume.ny();
            z = rand() % volume.nz();
        } while ( real.get_value_at(x, y, z) < density_threshold );


        double random_number = rand() / (double) RAND_MAX;        
        
        if( random_number < PDB_CARBON_FRACTION ) 
        {
            write_pdb_coordinate(out_file_stream, bead, "CA" , x, y, z);
        }
        else if( random_number < PDB_CARBON_FRACTION + PDB_NITROGEN_FRACTION ) 
        {
            write_pdb_coordinate(out_file_stream, bead, "N " , x, y, z);
        }
        else if( random_number < PDB_CARBON_FRACTION + PDB_NITROGEN_FRACTION + PDB_OXYGEN_FRACTION)
        {
            write_pdb_coordinate(out_file_stream, bead, "O " , x, y, z);
        }
        else
        {
            write_pdb_coordinate(out_file_stream, bead, "S " , x, y, z);
        }

    }

}

tdx::data::RealSpaceData tdx::utilities::BeadModelGenerator::generate_bead_model_volume(tdx::data::Volume input_volume)
{   
    tdx::data::RealSpaceData output_real(input_volume.nx(), input_volume.ny(), input_volume.nz());
    tdx::data::RealSpaceData oxygen = tdx::utilities::density_generator::create_density(11, max_resolution, 8.0);
    tdx::data::RealSpaceData carbon = tdx::utilities::density_generator::create_density(11, max_resolution, 6.0);
    tdx::data::RealSpaceData nitrogen = tdx::utilities::density_generator::create_density(11, max_resolution, 7.0);
    tdx::data::RealSpaceData sulphur = tdx::utilities::density_generator::create_density(11, max_resolution, 16.0);
    
    tdx::data::RealSpaceData input_real = input_volume.get_real();
    
    int x, y, z = 0;
    int max_trials = number_of_beads;
    int car = 0 ; 
    int nit = 0; 
    int oxy = 0; 
    int sul = 0;
    for ( int bead = 0; bead < number_of_beads; bead++ ) 
    {
        int trial = 0;
        do 
        {
            x = rand() % input_volume.nx();
            y = rand() % input_volume.ny();
            z = rand() % input_volume.nz();
            trial ++;
            if(trial > max_trials)
            {
                std::cerr << "Too many trials while generating bead model. May be density threshold too much or too many beads!!\n";
                exit(1);
            }
        } while ( input_real.get_value_at(x, y, z) < density_threshold);


        double random_number = rand() / (double) RAND_MAX;        
        
        if( random_number < PDB_CARBON_FRACTION ) 
        {
            output_real.merge_data(carbon, x, y, z);
            car++;
        }
        else if( random_number < PDB_CARBON_FRACTION + PDB_NITROGEN_FRACTION ) 
        {
            output_real.merge_data(nitrogen, x, y, z);
            nit++;
        }
        else if( random_number < PDB_CARBON_FRACTION + PDB_NITROGEN_FRACTION + PDB_OXYGEN_FRACTION)
        {
            output_real.merge_data(oxygen, x, y, z);
            oxy++;
        }
        else
        {
            output_real.merge_data(sulphur, x, y, z);
            sul++;
        }

    }
    
    std::cout << "\nNumber of beads\t:\t" << number_of_beads << "\n";
    std::cout << "Placed carbon\t:\t" << car << "\n";
    std::cout << "Placed nitrogen\t:\t" << nit << "\n";
    std::cout << "Placed oxygen\t:\t" << oxy << "\n";
    std::cout << "Placed sulphur\t:\t" << sul << "\n\n\n";
    
    return output_real;

}

void tdx::utilities::BeadModelGenerator::write_pdb_coordinate
        (std::ofstream& out_file_stream, int atom_id, std::string atom_type, int x, int y, int z)
{
    
    const int COORDINATE_WIDTH = 8;
    const int COORDINATE_PRECISION = 3;
    
    double sig = noise_level;
    
    /**
     * FORMAT IS:
     * 
        COLUMNS        DATA TYPE       CONTENTS                            
        --------------------------------------------------------------------------------
         1 -  6        Record name     "ATOM  "                                            

         7 - 11        Integer         Atom serial number.                   

        13 - 16        Atom            Atom name.                            

        17             Character       Alternate location indicator.         

        18 - 20        Residue name    Residue name.                         

        22             Character       Chain identifier.                     

        23 - 26        Integer         Residue sequence number.              

        27             AChar           Code for insertion of residues.       

        31 - 38        Real(8.3)       Orthogonal coordinates for X in Angstroms.                       

        39 - 46        Real(8.3)       Orthogonal coordinates for Y in Angstroms.                            

        47 - 54        Real(8.3)       Orthogonal coordinates for Z in Angstroms.                            

        55 - 60        Real(6.2)       Occupancy.                            

        61 - 66        Real(6.2)       Temperature factor (Default = 0.0)     
     */
    out_file_stream     << std::fixed
                        << "ATOM  "
                        << std::setw(5) << atom_id % 99999 << "" /* Make sure that id is not more than 99999*/
                        << std::setw(4) << atom_type
                        << "  ALA A"
                        << std::setw(4) << atom_id % 9999 << "    " /* Make sure that id is not more than 9999*/
                        << std::setw(COORDINATE_WIDTH) << std::setprecision(COORDINATE_PRECISION) << sig*2.0*(rand()/(double)RAND_MAX -0.5)+x
                        << std::setw(COORDINATE_WIDTH) << std::setprecision(COORDINATE_PRECISION) << sig*2.0*(rand()/(double)RAND_MAX -0.5)+y
                        << std::setw(COORDINATE_WIDTH) << std::setprecision(COORDINATE_PRECISION) << sig*2.0*(rand()/(double)RAND_MAX -0.5)+z
                        << "  1.00  0.00"
                        << std::endl;
}

void tdx::utilities::BeadModelGenerator::write_pdb_header
        (std::ofstream& out_file_stream, double a, double b, double c, double alpha, double beta, double gamma, std::string symmetry)
{
    
    const int ANGLE_WIDTH = 7;
    const int ANGLE_PRECISION = 2;
    
    const int LENGTH_WIDTH = 9;
    const int LENGTH_PRECISION = 3;
    
    /*
     * 
     COLUMNS       DATA TYPE      CONTENTS
    --------------------------------------------------------------------------------
     1 -  6       Record name    "CRYST1"

     7 - 15       Real(9.3)      a (Angstroms)

    16 - 24       Real(9.3)      b (Angstroms)     

    25 - 33       Real(9.3)      c (Angstroms)     

    34 - 40       Real(7.2)      alpha (degrees)   

    41 - 47       Real(7.2)      beta (degrees)    

    48 - 54       Real(7.2)      gamma (degrees)   

    56 - 66       LString        Space group       

    67 - 70       Integer        Z value         
     */
    out_file_stream << std::fixed
                    << "CRYST1"
                    << std::setw(LENGTH_WIDTH) << std::setprecision(LENGTH_PRECISION) << a
                    << std::setw(LENGTH_WIDTH) << std::setprecision(LENGTH_PRECISION) << b
                    << std::setw(LENGTH_WIDTH) << std::setprecision(LENGTH_PRECISION) << c
                    << std::setw(ANGLE_WIDTH) << std::setprecision(ANGLE_PRECISION) << alpha
                    << std::setw(ANGLE_WIDTH) << std::setprecision(ANGLE_PRECISION) << beta
                    << std::setw(ANGLE_WIDTH) << std::setprecision(ANGLE_PRECISION) << gamma
                    << std::setw(12) << symmetry
                    << "   1"
                    << std::endl;
            
}



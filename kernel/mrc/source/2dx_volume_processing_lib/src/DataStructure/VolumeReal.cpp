/*
 * Routines implementing the Real volume
 * 
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 */

#include "../../include/VolumeReal.hpp"

void VolumeReal::write_volume(const char* outFileName){
    std::FILE* outFile;
    
    outFile = fopen(outFileName, "w");
    for(std::map<Triplet2dx, WeightedDouble >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
        int h = (*ii).first.x();
        int k = (*ii).first.y();
        int l = (*ii).first.z();
        double value = (*ii).second.getValue();
        
        fprintf(outFile, "%5d %5d %5d %11.5f\n", h, k, l, value);
    }
    fclose(outFile);
}

void VolumeReal::array_data(const double* array){
    //Initialize the data
    _data.clear();
    
    //Loop over all possible indices and set their values
    for(int ix=0; ix<nx(); ix++){
        for(int iy=0; iy<ny(); iy++){
            for(int iz=0; iz<nz(); iz++){
                Triplet2dx id(ix, iy, iz);
                Triplet2dx arraySize = Triplet2dx(nx(), ny(), nz());
                int arrayId = id.get_array_id(arraySize);
                double arrayValue = array[arrayId];
                //std::cout << "Getting value at " << arrayId << " " << arrayValue << "\n";
                if(arrayValue > 0.0001 || arrayValue < -0.0001){
                    WeightedDouble wtdValue(arrayValue, 1.0);
                    set_value_at(ix, iy, iz, wtdValue);
                    //std::cout << "Setting value at " << arrayId << " " << arrayValue << "\n";
                }
            }
        }
    }
    
}

double* VolumeReal::array_data() const {
    double* array;
    Triplet2dx sizeTriplet(nx(), ny(), nz());
    int size = sizeTriplet.get_self_muliplication();
    array = (double*)malloc(size * sizeof(double));
    
    for(int i=0; i<size; i++){
        array[i]=0.0;
    }
    
    for(std::map<Triplet2dx, WeightedDouble>::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
        Triplet2dx index = (*ii).first;
        int arrayId = index.get_array_id(sizeTriplet);
        double value = (*ii).second.getValue();
        array[arrayId] = value;
        //std::cout << "reading value at " << arrayId << " " << value << "\n";
    }
    
    return array;
}

VolumeReal VolumeReal::membrane_slab(int membrane_height) {
    int cut_start = membrane_height/2;
    int cut_end = cut_start + (nz()-membrane_height);
    
    VolumeHeader header(get_header());
    VolumeReal newVolume(header);
    
    for(std::map<Triplet2dx, WeightedDouble>::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
        int z_id = (*ii).first.z();
        if( z_id < cut_start || z_id > cut_end) newVolume.set_value_at((*ii).first.x(), (*ii).first.y(), (*ii).first.z(), (*ii).second);
    }
    
    return newVolume;
}



/* 
 * File:   Volume2dx.hpp
 * Author: Nikhil Biyani (nikhilbiyani@gmail.com)
 *
 * Created on March 2, 2015, 4:04 PM
 */

#ifndef VOLUME2DX_HPP
#define	VOLUME2DX_HPP

#include <iostream>
#include <iomanip> 
#include <fstream>
#include <math.h>
#include <string.h>
#include <fftw3.h>
#include <map>
#include <list> 

#include "VolumeHeader.hpp"
#include "SystemUtils.hpp"
#include "WeightedValue.hpp"
#include "MillerIndex.hpp"
#include "NumericalUtils.hpp"
#include "FourierTransform.hpp"
#include "SymmetryOperations.hpp"

/* 
 * A class to provide all the required functionalities
 * of a 2D-Electron Crystallography volume.
 * Use this class for any instance of a Volume to be created in 2dx
 */
class Volume2dx{
public:
    /*================
     * Typedefs
     =================*/
    typedef std::map<MillerIndex, WeightedComplex> ReflectionMap;
    typedef std::multimap<MillerIndex, WeightedComplex> ReflectionMultiMap;
    typedef std::pair<MillerIndex, WeightedComplex> ReflectionPair;
    typedef std::list<WeightedComplex> WeightedComplexList;
    
    /*================
     * Constructors
     =================*/
    /*
     * Default Constructor
     */
    Volume2dx(){
        _header = new VolumeHeader2dx();
        _transform = new FourierTransform();
        _fourier = new ReflectionMap();
        _real = new double();
        _type = NONE;
    };
    
    /*
     * Constructor with size
     */
    Volume2dx(int nx, int ny, int nz){
        _header = new VolumeHeader2dx(nx, ny, nz);
        _transform = new FourierTransform();
        _fourier = new ReflectionMap();
        _real = (double*) calloc(nx * ny * nz, sizeof(double));
        _type = NONE;
    };
    
    /*
     * Copy constructor creating new pointers
     */
    Volume2dx(const Volume2dx& copy){
        //Initialize new pointers
        _header = new VolumeHeader2dx((*copy._header));
        _fourier = new ReflectionMap((*copy._fourier));
        _real = new double((*copy._real));
        _type = copy._type;
        _transform = copy._transform;
    }
    
    /*================
     * Operators
     =================*/
    
    /*
     * Operator Overloading of =
     * Returns volume with same pointers
     */
    Volume2dx& operator=(const Volume2dx& rhs){
        //Initialize with same pointers
        _header = rhs._header;
        _fourier = rhs._fourier;
        _real = rhs._real;
        _type = rhs._type;
    };
    
    /*=====================
     * Getters and Setters
     ======================*/
    
    /*
     * Returns the Complex value at the Miller index h, k, l 
     * If Fourier data not initialized, returns NULL
     */
    Complex2dx complex_value_at(int h, int k, int l) const{
        if(!(has_fourier())) throw("Complex data not set, still a request for data!!");
        WeightedComplex* wtdValue = new WeightedComplex();
        if(exists_in_map(h,k,l)){
            *wtdValue = _fourier->at(MillerIndex(h, k, l));
        }
        
        return wtdValue->getValue();
    };
    
    double complex_weight_at(int h, int k, int l) const{
        if(!(has_fourier())) throw("Complex data not set, still a request for data!!");
        WeightedComplex* wtdValue = new WeightedComplex();
        if(exists_in_map(h,k,l)){
            *wtdValue = _fourier->at(MillerIndex(h, k, l));
        }
        
        return wtdValue->getWeight();
    };
    
    /*=============================
     * Header getters and setters
     ===============================*/
    /*
     * Get the x dimension of the real volume
     */
    int nx() const{
        return _header->nx();
    };
    
    /*
     * Get the y dimension of the real volume
     */
    int ny() const{
        return _header->ny();
    };
    
    /*
     * Get the z dimension of the real volume
     */
    int nz() const{
        return _header->nz();
    };
    
    /*
     * Get the x dimension of the Fourier volume
     */
    int fx() const{
        return (int) (nx()/2+1);
    };
    
    /*
     * Get the y dimension of the Fourier volume
     */
    int fy() const{
        return ny();
    };
    
    /*
     * Get the z dimension of the Fourier volume
     */
    int fz() const{
        return nz();
    };
    
    /*
     * Returns the maximum possible value of miller index h
     */
    int h_max() const{
        return fx()-1;
    }
    
    /*
     * Returns the maximum possible value of miller index k
     */
    int k_max() const{
        return (int) fy()/2;
    }
    
    /*
     * Returns the maximum possible value of miller index l
     */
    int l_max() const{
        return (int) fz()/2;
    }
    
    double apix() const{
        return _header->apix();
    }
    
    /*
     * Returns the maximum possible value of the resolution set in header
     */
    double max_resolution() const {
        return _header->max_resolution();
    }
    
    /*
     * Returns the membrane height set in header
     */
    double membrane_height() const {
        return _header->membrane_height();
    }
    
    /*
     * Sets the x-dimension for real volume
     */
    void nx(int nx){
        if(_type == NONE) _header->nx(nx);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the y-dimension for real volume
     */
    void ny(int ny){
        if(_type == NONE) _header->ny(ny);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the z-dimension for real volume
     */
    void nz(int nz){
        if(_type == NONE) _header->nz(nz);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the symmetry of the volume
     */
    void symmetry(std::string symmetry){
        if(_type == NONE) _header->symmetry(symmetry);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the pixel size of the volume
     */
    void apix(double apix){
        if(_type == NONE) _header->apix(apix);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the max_resolution of the volume
     * @input: max_resolution (in Angstroms)
     */
    void max_resolution(double max_resolution){
        if(_type == NONE) _header->max_resolution(max_resolution);
        else throw(_read_only_header_string);
    };
    
    /*
     * Sets the membrane height of the volume
     * @input: membrane_height (height in fraction of nz)
     */
    void membrane_height(double membrane_height){
        if(_type == NONE) _header->membrane_height(membrane_height);
        else throw(_read_only_header_string);
    };
    
    
    /*=============================
     * Reading and Writing to Files
     ===============================*/
    
    /*
     * Function to read in the volume from a file
     * Supported Types: MRC, HKZ
     */
    void read_volume(std::string inFilePath){
        std::string extension = get_extension(inFilePath);
        read_volume(inFilePath, extension);
    };
    
    void read_volume(std::string inFilePath, std::string format){
        if(format == "mrc") read_mrc(inFilePath);
        if(format == "hkz") read_hkz(inFilePath);
        else throw("File format " + format + " not supported for reading.");
    };
    
    /*
     * Function to write the volume to a file
     * Supported Types: MRC, HKL
     */
    void write_volume(std::string outFilePath){
        std::string extension = get_extension(outFilePath);
        
        if(extension == "mrc") write_mrc(outFilePath);
        if(extension == "hkl") write_hkl(outFilePath);
        else throw("File format " + extension + " not supported for reading.");
    };
    
    
    /*=======================
     * Mathematical Operations
     ========================*/
    
    /*
     * Scale the values of the components by a factor
     
    void scale(double factor){
        typename std::map<Triplet2dx, WeightedComplex<T> >::iterator ii;
        for(ii=_data.begin(); ii!=_data.end(); ++ii){ 
                (*ii).second.setValue((*ii).second.getValue()*factor);
        }
    };
    
    
    /*=======================
     * Statistical Functions
     ========================*/
    
    /*
     * Returns the maximum of the volume
     
    WeightedComplex<T> get_maximum() const{
        WeightedComplex<T> max = (*_data.begin()).second;
        for(typename std::map<Triplet2dx, WeightedComplex<T> >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
            WeightedComplex<T> current = (*ii).second;
            if(max < current) max = current;
        }
        return max;
    };
    
    /*
     * Returns the minimum of the volume
     
    WeightedComplex<T> get_miminum() const{
        WeightedComplex<T> minval = (*_data.begin()).second;
        for(typename std::map<Triplet2dx, WeightedComplex<T> >::const_iterator ii=_data.begin(); ii!=_data.end(); ++ii){
            WeightedComplex<T> current = (*ii).second;
            if(current < minval) minval = current;
        }
        return minval;
    };
    */
    /*=======================
     * Utility Functions
     ========================*/
    
    /*
     * A function to return the resolution of the spot at (x, y, z)
     */
    double get_resolution(int x, int y, int z) const{
        double res[3];
        
        //Set the resolution to maximum
        res[0] = res[1] = res[2] = 1000;
        double nyquist = 1/(2*apix());

        if(x!=0) res[0] = nx()/(2*nyquist*x);
        if(y!=0) res[1] = ny()/(2*nyquist*y);
        if(z!=0) res[2] = nz()/(2*nyquist*z);

        return sqrt(res[0]*res[0] + res[1]*res[1] + res[2]*res[2]);
    };
    
    /*
     * Symmetrization
     */
    void symmetrize(){
        ReflectionMultiMap refMultiMap;

        if(!(has_fourier())) set_fourier();
        
        for(ReflectionMap::iterator ref=_fourier->begin(); ref!=_fourier->end(); ++ref){
            if((*ref).second.getValue().getAmplitude() > 0.0001){
                
                //Put in the original reflection
                refMultiMap.insert(ReflectionPair((*ref).first, (*ref).second));
                
                MillerIndex oldIndex = MillerIndex((*ref).first);
                WeightedComplex oldReflection = WeightedComplex((*ref).second);
                double oldAmplitude = oldReflection.getValue().getAmplitude();
                double oldPhase = oldReflection.getValue().getPhase();
                
                //Loop over all possible operations
                for(int opIndex=0; opIndex<30; opIndex++){
                    SymmetryOperations operation = SymmetryOperations(opIndex, _header->symmetry_code());
                    if(!(operation.skipOperation())){
                        //Get the symmetric miller index for this operator
                        MillerIndex newIndex = operation.getNewMiller(oldIndex);

                        //Get the corresponding new phase
                        double newPhase = operation.getPhaseChange(oldPhase, oldIndex);

                        //For negative h get Friedel spot, change phase
                        if(newIndex.h()<0){
                           newIndex = newIndex.getFriedelSpot();
                           newPhase = -1*newPhase;
                        }

                        double newReal = oldAmplitude*cos(newPhase);
                        double newImag = oldAmplitude*sin(newPhase);

                        WeightedComplex newReflection = WeightedComplex(Complex2dx(newReal, newImag), oldReflection.getWeight());

                        //std::cout << opIndex << "(" << oldIndex.h() << "," << oldIndex.k() << "," << oldIndex.l() <<")";
                        //std::cout << " -> (" << newIndex.h() << "," << newIndex.k() << "," << newIndex.l() <<")";
                        //std::cout << " phase " << (*ref).second.getValue().getPhase() << "->" << newReflection.getValue().getPhase() <<"\n";

                        
                        refMultiMap.insert(ReflectionPair(newIndex, newReflection));
                    }
                }
            }
        }

        //std::cout << "Out of loop\n";
        //delete _fourier;
        //delete[] _real;
        _type= FOURIER;
        _fourier = averageOutReflections(refMultiMap);
    };
    
    
private:
    /*=======================
     * Private Functions
     ========================*/
    /*
     * Function to check if the Fourier data is present in the memory
     */
    bool has_fourier() const {
        if(_type == FOURIER || _type == BOTH) return true;
        else return false;
    };
    
    /*
     * Function to check if the real data is present in the memory
     */
    bool has_real() const {
        if(_type == REAL || _type == BOTH) return true;
        else return false;
    };
    
    void free_data(){
        //delete[] _fourier;
        //delete[] _real;
    };
    
    /*
     * Function to check if a particular miller index is present in the map
     */
    bool exists_in_map(int h, int k, int l) const {
        if(!(has_fourier())){
            return false;
        }
        else if ( _fourier->find(MillerIndex(h, k, l)) == _fourier->end() ){
            return false;
        } 
        else {
            return true;
        }       
    };
    
    /*
     * Sets the value at index h, k, l
     */
    void set_complex_at(int h, int k, int l, Complex2dx value, double weight){
        MillerIndex index = MillerIndex(h, k, l);
        if(exists_in_map(h,k,l)) _fourier->erase(index);
        _fourier->insert(ReflectionPair(index, WeightedComplex(value, weight)));
    };
    
    /*=======================
     * Private I/O Functions
     ========================*/
    
    /*
     * Reads in an HKZ file with reflections put in the following format:
     * h    k   l   amplitude   phase   sig_amp sig_phase   iq_value
     * Initializes the data of the volume
     */
    void read_hkz(std::string inFilePath)
    {
        std::cout << "Reading HKZ File..\n";
        ReflectionMultiMap refMultiMap;
    
        int hIn, kIn, lIn, iqIn;
        double zIn, ampIn, phaseIn, sampIn, sphaseIn;

        std::ifstream hkzFile(inFilePath);

        //Start reading in the file
        while (hkzFile >> hIn >> kIn >> zIn >> ampIn >> phaseIn >> sampIn >> sphaseIn >> iqIn)
        {
            
            lIn = round(zIn * nz());
            double resolution =  get_resolution(hIn, kIn, lIn);

            //Convert phase to radians
            phaseIn = degree_to_radian(phaseIn);

            //Covert also the sphaseIn to radians
            if(sphaseIn>90) sphaseIn = 0;
            sphaseIn = degree_to_radian(sphaseIn);

            if(resolution > max_resolution())
            {
                double weightIn = cos(sphaseIn);
                double realIn = weightIn*ampIn*cos(phaseIn);
                double imagIn = weightIn*ampIn*sin(phaseIn);
                MillerIndex indexIn = MillerIndex(hIn, kIn, lIn);
                Complex2dx complexIn = Complex2dx(realIn, imagIn);
                WeightedComplex valueIn = WeightedComplex(complexIn, weightIn);
                //std::cout << indexIn.x() << " " <<indexIn.y() << " " << indexIn.z() << " : " << valueIn.getValue().getAmplitude() << valueIn.getValue().getAmplitude() << "\n";
                refMultiMap.insert(ReflectionPair(indexIn, valueIn));
            }

        }

        hkzFile.close();

        //free_data();
        _fourier = averageOutReflections(refMultiMap);
        _type = FOURIER;

        //Scale the amplitudes, so that maximum is 20,000
        //double currentMax = get_maximum().getValue().getAmplitude();
        //this->scale(20000/currentMax);
    };
    
    void read_mrc(std::string inFileName)
    {
        
    };
    
    /*
     * A function to write the volume in HKL format
     */
    void write_hkl(std::string outFileName)
    {
        std::cout << "Writing the hkl file.. \n";
        const int INT_WIDTH = 5;
        const int FLOAT_WIDTH = 13;
        const int FLOAT_PRECISION = 7;
        
        //If the Fourier data is not set, 
        if(!(has_fourier())) set_fourier();
        
        std::ofstream hklFile(outFileName);
        
        std::cout << "Writing the hkl file.. \n";
        
        for(ReflectionMap::const_iterator ii=_fourier->begin(); ii!=_fourier->end(); ++ii){
            int h = (*ii).first.h();
            int k = (*ii).first.k();
            int l = (*ii).first.l();
            double amp = (*ii).second.getValue().getAmplitude();
            double phase = ((*ii).second.getValue().getPhase())*180/M_PI;
            double fom = (*ii).second.getWeight()*100;

            hklFile << std::setw(INT_WIDTH) << h << " "
                    << std::setw(INT_WIDTH) << k << " "
                    << std::setw(INT_WIDTH) << l << " "
                    << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << amp << " "
                    << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << phase << " "
                    << std::setw(FLOAT_WIDTH) << std::setprecision(FLOAT_PRECISION) << fom << std::endl;
        }
        
        hklFile.close();
    };
    
    void write_mrc(std::string outFileName)
    {
        
    };
    
    
    /*===================================
     * Private Utility Functions
     ====================================*/
    
    /*
     * A function to set the Fourier data from real data
     */
    void set_fourier(){
        std::cout << "Setting the Fourier data.. \n";
        if(_type == REAL){
            delete[] _fourier;
            fftw_complex* complex_data = (fftw_complex*) malloc(fx()*fy()*fz()*sizeof(fftw_complex));
            _transform->real_to_complex(_real, complex_data, nx(), ny(), nz());
            from_fftw_complex(complex_data);
            _type = BOTH;
        }
        else if(_type == NONE) {
            throw("Hey, Fourier data cannot be set! Real data not in memory. Did you forget to set the data?");
        }
        
    }
    
    /*
     * A function to set the real data from Fourier data
     */
    void set_real(){
        if(_type == FOURIER){
            delete[] _real;
            double* real_data = (double*) malloc(nx()*ny()*nz()*sizeof(double));
            _transform->complex_to_real(to_fftw_complex(),real_data, nx(), ny(), nz());
            _type = BOTH;
            _real = real_data;
        }
        else if(_type == NONE) {
            throw("Hey, Real data cannot be set! Fourier data not in memory. Did you forget to set the data?");
        }
    }
    
    /*
     * Sets the Fourier data from fftw_complex
     */
    void from_fftw_complex(const fftw_complex* complex_data){
        _fourier->clear();
    
        //Calculate the size of the array
        Triplet2dx array_dim(fx(), fy(), fz());
        MillerIndex index_max(h_max(), k_max(), l_max());

        //Loop over all possible indices and set their values
        for(int ix=0; ix<array_dim.x(); ix++){
            for(int iy=0; iy<array_dim.y(); iy++){
                for(int iz=0; iz<array_dim.z(); iz++){
                    Triplet2dx currArray(ix, iy, iz);
                    int currId = currArray.get_array_id(array_dim);
                    double real = ((fftw_complex*)complex_data)[currId][0];
                    double imag = ((fftw_complex*)complex_data)[currId][1];

                    //Assign the current miller index
                    Complex2dx currentComplex(real, imag);
                    MillerIndex currentHKL(currArray, array_dim, index_max);
                    if(currentHKL.h() >= 0 && currentComplex.getAmplitude() > 0.0001){
                        this->set_complex_at(currentHKL.h(), currentHKL.k(), currentHKL.l(), currentComplex, 1.0);
                    }
                }
            }
        }
    };
    
    /*
     * A function to get the Fourier data in fftw_complex format
     */
    fftw_complex* to_fftw_complex() const {
    
        //Calculate the size of the array
        Triplet2dx array_dim(fx(), fy(), fz());
        int vol_size = array_dim.get_self_muliplication();

        fftw_complex* fftw_data;
        fftw_data = (fftw_complex*) malloc(vol_size*sizeof(fftw_complex));

        //Initialize the fftw_data with 0
        for(int i=0; i<vol_size; ++i){
            ((fftw_complex*)fftw_data)[i][0] = 0.0;
            ((fftw_complex*)fftw_data)[i][1] = 0.0;
        }

        //Iterate over all possible miller indices h, k, l
        for(ReflectionMap::const_iterator ref=_fourier->begin(); ref!=_fourier->end(); ++ref){

            //Assign the current Miller Index to the array
            MillerIndex currentHKL = (*ref).first;
            Complex2dx currentComplex = (*ref).second.getValue();
            // Fill in this spot
            if(currentHKL.h() >= 0){
                Triplet2dx indexTriplet = currentHKL.to_array_index(array_dim);
                int arrayId = indexTriplet.get_array_id(array_dim);
                ((fftw_complex*)fftw_data)[arrayId][0] = currentComplex.real();
                ((fftw_complex*)fftw_data)[arrayId][1] = currentComplex.imag();
            }
        }

        return fftw_data;

    };
    
    /*
     * A function which takes in a multimap where each Miller Index can have
     * multiple values of spots. This map is converted to a map where each Miller
     * index can has only one value.
     */
    ReflectionMap* averageOutReflections(const ReflectionMultiMap& refMultiMap) const {
        std::cout << "reading non averaged map.. \n";
        ReflectionMap* averagedMap = new ReflectionMap();
        bool initialized = false;

        MillerIndex currentIndex;
        WeightedComplexList reflectionsCurrent;

        for(ReflectionMultiMap::const_iterator ii=refMultiMap.begin(); ii!=refMultiMap.end(); ++ii){
            if(!(initialized)){
                currentIndex = (*ii).first;
                initialized = true;
            }

            if(!(currentIndex == (*ii).first)){
                WeightedComplex avgedReflection = averageReflectionsFromList(reflectionsCurrent);
                averagedMap->insert(ReflectionPair(currentIndex, avgedReflection));
                //std::cout << currentIndex.x() << " " << currentIndex.y() << " " << currentIndex.z() << "\n";
                //std::cout << " : " << avgedReflection.getValue().real() << "," << avgedReflection.getValue().imag();
                //WeightedComplex getback = averagedMap[currentIndex];
                //std::cout << " : " << getback.getValue().real() << "," << getback.getValue().imag() <<"\n";
                reflectionsCurrent.clear();
            }
            reflectionsCurrent.push_back((*ii).second);
            currentIndex = (*ii).first;

        }

        averagedMap->insert(ReflectionPair(currentIndex, averageReflectionsFromList(reflectionsCurrent))); 

        std::cout << "returning averaged map.. \n";
        return averagedMap;
    };

    /*
     * A function to average out values of possible reflections from list and 
     * returns one value
     */
    WeightedComplex averageReflectionsFromList(const WeightedComplexList values) const {
        WeightedComplex sumOverall;
        Complex2dx sumValues(0,0);
        double sumWeight = 0.0;
        double sumXarg = 0.0;
        for(WeightedComplexList::const_iterator i=values.begin(); i != values.end(); ++i){
            //std::cout << (*i).getValue().real() << " " << (*i).getValue().imag() <<"\n" ;
            sumWeight += (*i).getWeight();
            sumXarg += fomToXarg((*i).getWeight());
            sumValues = sumValues + (*i).getValue();
        }

        if(sumXarg > 54) sumXarg = 54;
        double xargAvg = i1(sumXarg)/i0(sumXarg);

        //std::cout << "sums: " << sumWeight << " " << xargAvg << " (" << sumValues.real() << "," << sumValues.imag() << ")\n";

        if(sumWeight!=0 && values.size()!=0){
            //std::cout << "In loop\n";
            sumValues = sumValues*(xargAvg/sumWeight);
            //std::cout << " (" << sumValues.real() << "," << sumValues.imag() << ")\n";
            sumOverall.setValue(sumValues);
            sumOverall.setWeight(xargAvg);
        }

        //std::cout << "-------------\n" << sumOverall.getValue().real() << " " << sumOverall.getValue().imag() << "\n--------------\n";
        return sumOverall;  
    };
    
    
private:
    /*
     * ENUM to define data type of class
     * NONE: Nothing in memory/ Nothing initialized
     * REAL: Real data initialized
     * FOURIER: Fourier data initialized
     * BOTH: Both data are initialized and in memory
     */
    enum VolumeDataType {
        NONE=0, REAL=1, FOURIER=2, BOTH=3
    };
    
    /*=======================
     * Members
     ========================*/
    /*
     * Stores in all the necessary information about the volume
     */
    VolumeHeader2dx* _header;
    
    /*
     * Data variable to store Fourier data
     */
    ReflectionMap* _fourier;
    
    /*
     * Data variable to store real space data
     * NOTE: Origin is on the top left corner, sections going down
     */
    double* _real;
    
    /*
     * Variable to see what's (Fourier/real) initialized!
     */
    VolumeDataType _type;
    
    /*
     * Fourier Transform instance to perform Fourier transforms
     */
    FourierTransform* _transform;
    
    /*
     * String for illegal header setting
     */
    const std::string _read_only_header_string = 
        "Runtime Error: Hey! Header values being set after the data allocation is now allowed.\n";
    
};

#endif	/* VOLUME2DX_HPP */

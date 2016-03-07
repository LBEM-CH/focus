/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef FOURIERSPACEDATA_HPP
#define	FOURIERSPACEDATA_HPP

#include <map>
#include <list>
#include <fftw3.h>

#include "miller_index.hpp"
#include "peak_data.hpp"

namespace tdx
{
    namespace data
    {
        
        /**
         * A map of Miller Index and diffraction spot. Stores sorted values 
         * of a miller index mapped to diffraction spots.
         */
        typedef std::map<MillerIndex, PeakData> MillerToPeakMap;
        
        /**
         * A multi-map of Miller Index and Diffraction spot. 
         * Stores multiple diffraction spots at one miller index. 
         */
        typedef std::multimap<MillerIndex, PeakData> MillerToPeakMultiMap;
        
        /**
         * Defines a map of MillerIndex and double
         */
        typedef std::map<MillerIndex, double> MillerToDoubleMap;
        
        /**
         * Defines a pair of Miller Index and Diffraction Spot.
         */
        typedef std::pair<MillerIndex, PeakData> MillerToPeakPair;
        
        /**
         * Defines a pair of Miller Index and double.
         */
        typedef std::pair<MillerIndex, double> MillerToDoublePair;
        
        /**
         * A class to store Fourier space reflections HKL and values.
         * The data is stored as a map of miller indices and it's values.
         * Miller index is sorted with x changing slowest and z changing
         * fastest. For each miller index a complex value and it's weight
         * is also stored.
         */
        class ReflectionData
        {
            
        public:
            
            /**
             * Defines a const_iterator for the class
             */
            typedef MillerToPeakMap::const_iterator const_iterator;
            
            /**
             * Default constructor initializing the data
             */
            ReflectionData();
            
            /**
             * Copy constructor
             * @param copy - Instance to be copied from.
             */
            ReflectionData(const ReflectionData& copy);
            
            /**
             * Destructor
             */
            ~ReflectionData();
            
            /**
             * Operator definition of =
             * Copies the data from rhs and changes the current instance. 
             * Behavior similar to copy constructor
             * @param rhs
             */
            ReflectionData& operator=(const ReflectionData& rhs);
            
            /**
             * Operator definition of +
             * Joins the reflections from both rhs and current instance. For the
             * intersection set (reflections present in both) adds the reflections.
             */
            ReflectionData operator+(const ReflectionData& rhs);
            
            /**
             * Operator definition of * with double factor.
             * Multiplies the amplitudes of all reflections by the factor.
             * @param factor
             */
            ReflectionData operator*(double factor);
            
            /**
             * Clears the current data
             */
            void clear();
            
            /**
             * Resets (Copies) the data with another one
             * @param data
             */
            void reset(const ReflectionData& data);
            
            /**
             * Resets (Copies) the data with another one
             * @param data
             */
            void reset(const MillerToPeakMap& data);
            
            /**
             * Returns the beginning of the data for iterating
             * @return const_iterator
             */
            const_iterator begin() const;
            
            /**
             * Returns the beginning of the data for iterating
             * @return const_iterator
             */
            const_iterator end() const;
            
            /**
             * Overwrites the value of a miller index.
             * Internally inserts the miller index and it's value to the map.
             * @param h
             * @param k
             * @param l
             * @param value - complex value
             * @param weight - fom weight in fraction 
             */
            void set_spot_at(int h, int k, int l, Complex value, double weight);
            
            /**
             * Fetches the complex number stored at miller index h, k, l.
             * If found returns the complex, otherwise returns 0 + 0i
             * @param h
             * @param k
             * @param l
             * @return complex value at h, k, l
             */
            Complex value_at(int h, int k, int l) const ;
            
            /**
             * Fetches the weight at the miller index h, k, l.
             * If found returns the value, other wise returns 0.
             * @param h
             * @param k
             * @param l
             * @return 
             */
            double weight_at(int h, int k, int l) const ;
            
            /**
             * Checks if a miller index exists in map
             * @param h
             * @param k
             * @param l
             * @return True/False
             */
            bool exists(int h, int k, int l) const ;
            
            /**
             * Evaluates the sum of intensities and returns it
             * @return sum of intensities
             */
            double intensity_sum() const;
            
            /**
             * Evaluates and returns the number of spots in the Fourier space
             * @return number of spots
             */
            int spots() const;
            
            /**
             * Fetches the max_amplitude value
             * @return max_amplitude
             */
            double max_amplitude() const;
            
            /**
             * Scales the amplitudes by a factor
             * @param factor
             */
            void scale_amplitudes(double factor);
            
            /**
             * Converts the data into fftw format
             * @param fx : x size of the half Fourier volume
             * @param fy : y size of the half Fourier volume
             * @param fz : z size of the half Fourier volume
             * @return fftw_complex 
             */
            fftw_complex* fftw_data(int fx, int fy, int fz) const;
            
            /**
             * Resets the data from the fftw format
             * @param fx : x size of the half Fourier volume
             * @param fy : y size of the half Fourier volume
             * @param fz : z size of the half Fourier volume
             * @param complex_data
             */
            void reset_data_from_fftw(int fx, int fy, int fz, fftw_complex* complex_data);
            
            /**
             * Replaces the reflections present in input data. The reflections which are not
             * present in the input data will not be replaced. If one wants to replace the reflections
             * only with a certain minimum amplitude, this can also be set.
             * @param input - input reflections
             * @param cone_angle (in degrees) - (Default 90 degrees), if provided would keep the input reflections and would
             *                                   change other reflections in the conical region provided
             * @param replacement_amplitude_cutoff - Reflections in input with this amplitude value or more will only be changed
             * @return new replaced Fourier Space Data
             */
            void replace_reflections(const ReflectionData& input, double cone_angle = 90, double replacement_amplitude_cutoff = 0.0);
            
            /**
             * Replaces the amplitudes present in input data. The amplitudes which are not
             * present in the input data will not be replaced. If one wants to replace the amplitude
             * only with a certain minimum amplitude, this can also be set.
             * @param input - input reflections
             * @param replacement_amplitude_cutoff - Reflections in input with this amplitude value or more will only be changed
             * @return new replaced Fourier Space Data
             */
            void change_amplitudes(const ReflectionData& input, double replacement_amplitude_cutoff = 0.0);
            
            /**
             * Spreads the current data and tries to fill in the missing spots.
             */
            void spread_data();
            
            /**
             * Returns the full Fourier space with negative h as well
             * @return Fourier Space Data
             */
            ReflectionData get_full_fourier() const;
            
            /**
             * Returns the inverted Fourier data in a specific direction
             * @param direction = 0 for all axis (default), 1 for x axis invert, 2 for y axis invert, 3 for z axis invert, 
             * @return Data with inverted hand
             */
            ReflectionData inverted_data(int direction=0) const;
            
        private:
            
           
            
            /**
             * Data stored as a map of Miller Index and it's value
             */
            MillerToPeakMap _data;
            
            
        }; // class FourierSpaceData
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* FOURIERSPACEDATA_HPP */


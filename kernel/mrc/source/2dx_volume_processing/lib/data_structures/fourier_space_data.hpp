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
#include "diffraction_spot.hpp"

namespace volume
{
    namespace data
    {
        /**
         * A class to store Fourier space data.
         * The data is stored as a map of miller indices and it's values.
         * Miller index is sorted with x changing slowest and z changing
         * fastest. For each miller index a complex value and it's weight
         * is also stored.
         */
        class FourierSpaceData
        {
            
        public:
            
            /**
             * Defines a const_iterator for the class
             */
            typedef std::map<MillerIndex, DiffractionSpot>::const_iterator const_iterator;
            
            /**
             * Default constructor initializing the data
             */
            FourierSpaceData();
            
            /**
             * Constructor using a multi-map of miller index and diffraction spot
             * Internally converts a multi-map of miller index to diffraction spots
             * to a map of miller index to diffraction spot. In the input
             * multi-map, there can exist multiple diffraction spots assigned to
             * one miller index, which will be converted to a map where only
             * one diffraction spot is assigned to a miller index.
             * @param spot_multimap: A multi-map with miller indices mapped to multiple complexes
             */
            FourierSpaceData(const std::multimap<MillerIndex, DiffractionSpot>& spot_multimap);
            
            /**
             * Resets the data.
             */
            void reset();
            
            /**
             * Resets the data with another one
             * @param data
             */
            void reset(const FourierSpaceData& data);
            
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
            void set_value_at(int h, int k, int l, Complex2dx value, double weight);
            
            /**
             * Fetches the complex number stored at miller index h, k, l.
             * If found returns the complex, otherwise returns 0 + 0i
             * @param h
             * @param k
             * @param l
             * @return complex value at h, k, l
             */
            Complex2dx complex_at(int h, int k, int l) const ;
            
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
             * Replaces the reflections from the input Fourier space data to the current one.
             * The original reflection can be also be kep and the new reflection can be 
             * applied using only a fraction. fraction=1 will completely change the reflections.
             * @param input
             * @param fraction
             */
            void replace_reflections(const FourierSpaceData& input, double fraction);
            
            /**
             * Spreads the current data and tries to fill in the missing spots.
             */
            void spread_data();
            
            /**
             * Returns the full Fourier space with negative h as well
             * @return Fourier Space Data
             */
            FourierSpaceData get_full_fourier() const;
            
            /**
             * Returns the inverted hand Fourier data
             */
            FourierSpaceData invert_hand() const;
            
        private:
            
            /**
             * Data stored as a map of Miller Index and it's value
             */
            std::map<MillerIndex, DiffractionSpot>* _data;
            
            
        }; // class FourierSpaceData
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* FOURIERSPACEDATA_HPP */


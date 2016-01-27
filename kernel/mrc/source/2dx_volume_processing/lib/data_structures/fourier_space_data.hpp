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

namespace tdx
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
             * Copy constructor
             * @param copy - Instance to be copied from.
             */
            FourierSpaceData(const FourierSpaceData& copy);
            
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
             * Destructor
             */
            ~FourierSpaceData();
            
            /**
             * Operator definition of =
             * Copies the data from rhs and changes the current instance. 
             * Behavior similar to copy constructor
             * @param rhs
             */
            FourierSpaceData& operator=(const FourierSpaceData& rhs);
            
            /**
             * Operator definition of +
             * Joins the reflections from both rhs and current instance. For the
             * intersection set (reflections present in both) adds the reflections.
             */
            FourierSpaceData operator+(const FourierSpaceData& rhs);
            
            /**
             * Operator definition of * with double factor.
             * Multiplies the amplitudes of all reflections by the factor.
             * @param factor
             */
            FourierSpaceData operator*(double factor);
            
            /**
             * Clears the current data
             */
            void clear();
            
            /**
             * Resets (Copies) the data with another one
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
            void set_value_at(int h, int k, int l, Complex value, double weight);
            
            /**
             * Fetches the complex number stored at miller index h, k, l.
             * If found returns the complex, otherwise returns 0 + 0i
             * @param h
             * @param k
             * @param l
             * @return complex value at h, k, l
             */
            Complex complex_at(int h, int k, int l) const ;
            
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
            void replace_reflections(const FourierSpaceData& input, double cone_angle = 90, double replacement_amplitude_cutoff = 0.0);
            
            /**
             * Replaces the amplitudes present in input data. The amplitudes which are not
             * present in the input data will not be replaced. If one wants to replace the amplitude
             * only with a certain minimum amplitude, this can also be set.
             * @param input - input reflections
             * @param replacement_amplitude_cutoff - Reflections in input with this amplitude value or more will only be changed
             * @return new replaced Fourier Space Data
             */
            void change_amplitudes(const FourierSpaceData& input, double replacement_amplitude_cutoff = 0.0);
            
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
             * Returns the inverted Fourier data in a specific direction
             * @param direction = 0 for all axis (default), 1 for x axis invert, 2 for y axis invert, 3 for z axis invert, 
             * @return Data with inverted hand
             */
            FourierSpaceData inverted_data(int direction=0) const;
            
        private:
            
            /**
             * Back projects the multi-map of miller index and diffraction spot.
             * Internally converts a multi-map of miller index to diffraction spots
             * to a map of miller index to diffraction spot. In the input
             * multi-map, there can exist multiple diffraction spots assigned to
             * one miller index, which will be converted to a map where only
             * one diffraction spot is assigned to a miller index.
             * @param spot_multimap
             * @return map of MillerIndex and DiffractionSport
             */
            std::map<MillerIndex, DiffractionSpot> backproject(const std::multimap<MillerIndex, DiffractionSpot>& spot_multimap) const;
            
            /**
             * Data stored as a map of Miller Index and it's value
             */
            std::map<MillerIndex, DiffractionSpot> _data;
            
            
        }; // class FourierSpaceData
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* FOURIERSPACEDATA_HPP */


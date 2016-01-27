/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef STRUCTURE_FACTOR_HPP
#define	STRUCTURE_FACTOR_HPP

#include <iostream>
#include <string>

namespace tdx
{
    namespace data
    {
        class Volume;
        
        /**
         * A class used to store structure factors. Structure factors are radial
         * distribution of intensities in Fourier space. 
         */
        class StructureFactors
        {
        public:
            
            /**
             * Instance of the structure factors with min_resolution, 
             * max_resolution in frequency (1/A) units and resolution_bins assigned 
             * @param min_resolution - min resolution in 1/A units
             * @param max_resolution - max resolution in 1/A units
             * @param resolution_bins - number of bins used
             */
            StructureFactors(double min_resolution, double max_resolution, int resolution_bins);
            
            /**
             * Calculates the intensities in the bins and stores them.
             * @param volume: input volume for which the intensities will be calculated
             */
            void initialize_intensities(const Volume& volume);
            
            /**
             * A string converter of the class instance. 
             * Can be used to output the data present.
             * @return 
             */
            std::string to_string() const;
            
            /**
             * Returns a plot of the current structure factors 
             * ( i.e. resolution vs intensities )
             * @return the string containing the plot
             */
            std::string plot_profile() const;
            
            /**
             * Fetches the intensity value at the resolution provided
             * @param resolution - in frequency units (1/A)
             * @return double intensity value at resolution
             *         Returns -1.0, if found inappropriate resolution value
             */
            double intensity_at(double resolution) const;
            
            /**
             * Returns the value of minimum resolution set.
             * @return min resolution in 1/A units
             */
            double min_resolution() const;
            
             /**
             * Returns the value of maximum resolution set.
             * @return max resolution in 1/A units
             */
            double max_resolution() const;
            
            /**
             * Returns the number of bins used to generate intensities
             * @return number of bins
             */
            int resolution_bins() const;
            
            /**
             * Fetches the resolution spacing in 1/A which was used 
             * to generate the data
             * @return spacing in resolution (units 1/A)
             */
            double resolution_spacing() const;
            
            
            /**
             * Fetches the maximum intensity value
             * @return maximum intensity
             */
            double max_intensity() const;
            
        private:
            
            /**
             * Checks if the intensities have been initialized.
             * @return True: for initialized intensities/ otherwise False.
             */
            bool initialized() const;
            
            /**
             * Fetched the correct bin number (starting 0) for the given
             * resolution
             * @param resolution
             * @return bin number starting with 0,1,..,resolution_bins-1
             *         for inappropriate resolution values returns -1
             */
            int get_resolution_bin(double resolution) const;
            
            
            /**
             * Value of minimum resolution in frequency (1/A) units
             */
            double _min_resolution;
            
            /**
             * Value of maximum resolution in frequency (1/A) units
             */
            double _max_resolution;
            
            /**
             * Number of resolution bins used to generate the data
             */
            int _resolution_bins;
            
            /**
             * Array of size <resolution_bins> used to store
             * average intensities in each bin
             */
            double* _intensities;
            
        };
        
    }
    
}


#endif	/* STRUCTURE_FACTOR_HPP */


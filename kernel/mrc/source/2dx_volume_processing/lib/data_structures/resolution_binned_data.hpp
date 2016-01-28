/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef RESOLUTION_BINNED_DATA_HPP
#define	RESOLUTION_BINNED_DATA_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

namespace tdx
{
    namespace data
    {   
        /**
         * A class used to store resolution binned data.
         * This class can be used to store the data and later plot the sums
         * or the averages of the stored data.
         */
        class ResolutionBinnedData
        {
        public:
            
            /**
             * Instance of the structure factors with min_resolution, 
             * max_resolution in frequency (1/A) units and resolution_bins assigned 
             * @param min_resolution - min resolution in 1/A units
             * @param max_resolution - max resolution in 1/A units
             * @param resolution_bins - number of bins used
             */
            ResolutionBinnedData(double min_resolution, double max_resolution, int resolution_bins);
            
            /**
             * Writes into a file the resolution bins (1st column) 
             * and summed data (2nd column)
             * @param file - output file
             */
            void write_sum(std::string file) const;
            
            /**
             * Writes into a file the resolution bins (1st column) 
             * and averaged data (2nd column)
             * @param file - output file
             */
            void write_average(std::string file) const;
            
            /**
             * Writes into a file the resolution bins (1st column) 
             * and data (2nd column)
             * @param file - file name
             * @param average - should the data be averaged
             */
            void write(std::string file, bool average) const;
            
            /**
             * Returns a string containing a printable histogram of the
             * sum of data vs resolution bins
             * @return string with histogram 
             */
            std::string plot_sum() const;
            
            /**
             * Returns a string containing a printable histogram of the
             * average of data vs resolution bins
             * @return string with histogram 
             */
            std::string plot_average() const;
            
            /**
             * Returns a plot of the current data. (i.e. resolution vs data)
             * @param average - should the data be averaged
             * @return the string containing the histogram
             */
            std::string plot_profile(bool average) const;
            
            /**
             * Fetches the summed value at the resolution provided
             * @param resolution - in frequency units (1/A)
             * @return double value at provided resolution
             *         Returns -1.0, if found inappropriate resolution value
             */
            double sum_at(double resolution) const;
            
            /**
             * Fetches the averaged value at the resolution provided
             * @param resolution - in frequency units (1/A)
             * @return double value at provided resolution
             *         Returns -1.0, if found inappropriate resolution value
             */
            double average_at(double resolution) const;
            
            /**
             * Fetches the sum of the data in the provided bin
             * @param bin number starting from 0
             * @return sum in bin
             *         for inappropriate bins returns -1
             */
            double sum_in(int bin) const;
            
            /**
             * Fetches the average of the data in the provided bin
             * @param bin number starting from 0
             * @return average in bin
             *         for inappropriate bins returns -1
             */
            double average_in(int bin) const;
            
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
             * Fetched the correct bin number (starting 0) for the given
             * resolution
             * @param resolution
             * @return bin number starting with 0,1,..,resolution_bins-1
             *         for inappropriate resolution values returns -1
             */
            int get_resolution_bin(double resolution) const;
            
            /**
             * Sets the value at the resolution provided in correct bin
             * @param resolution - in frequency units (1/A)
             * @param value - value to be added
             */
            void add_data_at(double resolution, double value);
            
            /**
             * Fetches the maximum summed value
             * @return maximum value
             */
            double max_summed_value() const;
            
            /**
             * Fetches the maximum summed value
             * @return maximum value
             */
            double max_averaged_value() const;
            
        private:
               
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
             * data in each bin
             */
            double* _data;
            
            /**
             * Keeps a track of all the number of data points went to each bin
             */
            int* _counts;
            
        };
        
    }
    
}


#endif	/* RESOLUTION_BINNED_DATA_HPP */


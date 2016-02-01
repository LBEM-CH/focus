/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef BINNED_DATA_HPP
#define	BINNED_DATA_HPP

#include <iostream>
#include <fstream>
#include <string>
#include <cmath>

namespace tdx
{
    namespace data
    {   
        /**
         * A class used to store binned data.
         * The data are provided at the x-points and the class will keep track
         * of sums and counts in each bin
         * This class can be used to store the data and later plot the sums
         * or the simple averages of the stored data.
         */
        class BinnedData
        {
        public:
            
            /**
             * Constructor with min, max range and number of bins assigned 
             * @param min_range - min range
             * @param max_range - max range
             * @param bins - number of bins used
             */
            BinnedData(double min_range, double max_range, int bins);
                       
            /**
             * Sets the value at the point provided
             * @param data_point - the point at which the value is to be set
             * @param data_value - value to be added
             */
            void add_data_at(double data_point, double data_value);
            
            /**
             * Replaces the current sum in the bin with the one provided
             * @param bin number starting from 0,...,number_of_bins-1
             * @param sum
             */
            void set_bin_sum(int bin, double sum);
            
            /**
             * Replaces the current count in the bin with the one provided
             * @param bin number starting from 0,...,number_of_bins-1
             * @param count
             */
            void set_bin_count(int bin, int count);
            
            /**
             * Writes into a file the bins (1st column) 
             * and summed data (2nd column)
             * @param file - output file
             */
            void write_sum(std::string file) const;
            
            /**
             * Writes into a file the bins (1st column) 
             * and averaged data (2nd column)
             * @param file - output file
             */
            void write_average(std::string file) const;
            
            /**
             * Writes into a file the bins (1st column) 
             * and data (2nd column)
             * @param file - file name
             * @param average - should the data be averaged
             */
            void write(std::string file, bool average) const;
            
            /**
             * Returns a string containing a printable histogram of the
             * sum of data verses bin values
             * @return string with histogram 
             */
            std::string plot_sum() const;
            
            /**
             * Returns a string containing a printable histogram of the
             * average of data verses bin values
             * @return string with histogram 
             */
            std::string plot_average() const;
            
            /**
             * Returns a plot of the current data. (i.e. bins vs data)
             * @param average - should the data be averaged
             * @return the string containing the histogram
             */
            std::string plot_profile(bool average) const;
            
            /**
             * Fetches the summed value at the data point provided
             * @param data_point - where the sum is to be fetched
             * @return double value at provided point
             *         Returns -1.0, if found inappropriate value
             */
            double sum_at(double data_point) const;
            
            /**
             * Fetches the averaged value at the data point provided
             * @param data_point - where the sum is to be fetched
             * @return double value at provided data point
             *         Returns -1.0, if found inappropriate value
             */
            double average_at(double data_point) const;
            
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
             * Returns the value of minimum range set.
             * @return min range set
             */
            double min_range() const;
            
             /**
             * Returns the value of maximum range set.
             * @return max range set
             */
            double max_range() const;
            
            /**
             * Returns the number of bins used to generate intensities
             * @return number of bins
             */
            int bins() const;
            
            /**
             * Fetches the bin spacing which was used 
             * to generate the data
             * @return spacing
             */
            double spacing() const;
            
            /**
             * Fetches the correct bin number (starting 0) for the given
             * data point
             * @param data point
             * @return bin number starting with 0,1,..,bins-1
             *         for inappropriate point values returns -1
             */
            int get_bin_number(double data_point) const;
            
            /**
             * Fetches the maximum summed value
             * @return maximum value
             */
            double max_summed_value() const;
            
            /**
             * Fetches the maximum averaged value
             * @return maximum value
             */
            double max_averaged_value() const;
            
        private:
               
            /**
             * Value of minimum range
             */
            double _min_range;
            
            /**
             * Value of maximum range
             */
            double _max_range;
            
            /**
             * Number of bins used to generate the data
             */
            int _bins;
            
            /**
             * Array of size <bins> used to store
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


#endif	/* BINNED_DATA_HPP */


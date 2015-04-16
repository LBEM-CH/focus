/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#ifndef REAL_SPACE_DATA_HPP
#define	REAL_SPACE_DATA_HPP

namespace volume
{
    namespace data
    {
        /**
         * A class to sore real space data in a double array format.
         */
        class RealSpaceData
        {
        public:
            
            /**
             * Default constructor initializing a 0 size data
             */
            RealSpaceData();
            
            /**
             * Constructor initializing the data with given size
             * @param nx
             * @param ny
             * @param nz
             */
            RealSpaceData(int nx, int ny, int nz);
            
            /**
             * Resets the data with the given size
             * @param nx
             * @param nx
             * @param nz
             */
            void reset(int nx, int ny, int nz);
            
            /**
             * Resets the data. make sure that the size of the data is equals
             * the size of the class
             * @param data
             */
            void reset_data(double* data);
            
            /**
             * Returns the pointer to the data
             * @return pointer to the data
             */
            double* get_data() const;
            
            /**
             * Fetches the value at the given index
             * @param x
             * @param y
             * @param z
             * @return value
             */
            double get_value_at(int x, int y, int z) const ;
            
             /**
             * Fetches the value at the given index
             * @param id
             * @return value
             */
            double get_value_at(int id) const ;
            
            /**
             * Assigns the value to the given index
             * @param x
             * @param y
             * @param z
             * @param value
             */
            void set_value_at(int x, int y, int z, double value);
            
            /**
             * Assigns the value to the given index
             * @param id
             * @param value
             */
            void set_value_at(int id, double value);
            
            /**
             * The the current size of the array
             * @return size
             */
            long size() const ;
            
             /**
             * Fetches the memory location of the x, y, z
             * @param x
             * @param y
             * @param z
             * @return 
             */
            int memory_id(int x, int y, int z) const ;
            
            
            
        private:
            /**
             * A member initializer function with the size integers
             * @param nx
             * @param ny
             * @param nz
             */
            void initialize(int nx, int ny, int nz);
            
            
            /**
             * Checks if the indices are correct with the given data!
             * Should bypass the seg fault 11!
             * @param x
             * @param y
             * @param z
             * @return True for correct, false otherwise
             */
            bool indices_in_limit(int x, int y, int z) const ;
            
            /**
             * Data variable as a pointer to the double array
             */
            double* _data;
            
            /**
             * The size of the current data variable
             */
            int _nx, _ny, _nz;
            
            
        }; //class RealSpaceData
        
    } // namespace data_structures
    
} // namespace volume_processing_2dx

#endif	/* REAL_SPACE_DATA_HPP */


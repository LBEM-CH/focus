/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */

#ifndef SYMMETRY_OPERATIONS_HPP
#define	SYMMETRY_OPERATIONS_HPP

#include <stdexcept>
#include <math.h>

namespace tdx
{
    namespace symmetrization
    {
        
        /**
         * A generic class to handle all the symmetry operations that can occur in 
         * 2D-crystallography.
         * Considering all the available symmetries, there exists 30 different 
         * ways which can give us new symmetric spot. Each spot will vary
         * from the existing one in it's Miller Index and phase. 
         * These changes can be listed using the following table:
         * 
         *
         *   Op  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 
         *   H= -h +h -h +k +k -k -k +h -h +k -k -h +h -h +h -h +h -h +k +k -k -k +h -h +k -k -h +h -h +h
         *                                       -k +k -k +k                                  -k +k -k +k
         *   K= +k -k -k +h -h +h -h -h +h -h +h +h -h +k -k +k -k -k +h -h +h -h -h +h -h +h +h -h +k -k
         *                           -k +k -k +k                                  -k +k -k +k            
         *   L= +l +l +l +l +l +l +l +l +l +l +l +l +l +l +l -l -l -l -l -l -l -l -l -l -l -l -l -l -l -l
         * 
         * Phase changes in the following way:
         *   Codes:
         *   -- : Not comparable
         *   1  : Directly Identical
         *   H  : Differ by 180 * H
         *   K  : Differ by 180 * K
         *   L  : Differ by 180 * L
         *   HK : Differ by 180 * (H+K)
         * 
         * 
         *   Symmetry   0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29
         *   p1        -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
         *   p2        -- --  1 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- not sure
         *   p12       -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  1 -- -- -- -- -- -- -- -- -- -- -- -- -- --
         *   p121      -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  K -- -- -- -- -- -- -- -- -- -- -- -- -- --   
         *   c12       -- -- -- -- -- -- -- -- -- -- -- -- -- -- --  1 -- -- -- -- -- -- -- -- -- -- -- -- -- -- what does +(1/2, 1/2, 0) mean?
         *   p222      -- --  1 -- -- -- -- -- -- -- -- -- -- -- --  1  1 -- -- -- -- -- -- -- -- -- -- -- -- --  
         *   p2221     -- --  L -- -- -- -- -- -- -- -- -- -- -- --  L  1 -- -- -- -- -- -- -- -- -- -- -- -- -- not sure - other option is 15=1 & 16=L
         *   p22121    -- --  1 -- -- -- -- -- -- -- -- -- -- -- -- HK HK -- -- -- -- -- -- -- -- -- -- -- -- -- 
         *   c222      -- --  1 -- -- -- -- -- -- -- -- -- -- -- --  1  1 -- -- -- -- -- -- -- -- -- -- -- -- -- what does +(1/2, 1/2, 0) mean?
         *   p4        -- --  1 --  1  1 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
         *   p422      -- --  1 --  1  1 -- -- -- -- -- -- -- -- --  1  1 --  1 -- --  1 -- -- -- -- -- -- -- --
         *   p4212     -- --  1 -- HK HK -- -- -- -- -- -- -- -- -- HK HK --  1 -- --  1 -- -- -- -- -- -- -- --
         *   p3        -- -- -- -- -- -- -- -- --  1 --  1 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
         *   p312      -- -- -- -- -- -- -- -- --  1 --  1 -- -- -- -- -- -- -- -- --  1 --  1 -- -- -- -- --  1
         *   p321      -- -- -- -- -- -- -- -- --  1 --  1 -- -- -- -- -- --  1 -- -- --  1 -- -- -- -- --  1 --
         *   p6        -- --  1 -- -- -- -- -- --  1  1  1  1 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
         *   p622      -- --  1 -- -- -- -- -- --  1  1  1  1 -- -- -- -- --  1 -- --  1  1  1 -- -- -- --  1  1  
         * 
         */
        class SymmetryOperations
        {
            
        public:
            
            /**
             * Constructor with operator index and symmetry code.
             * @param operator_index index of the operator ranging from 0-29
             * @param symmetry_code code of the symmetry ranging from 0-16
             */
            SymmetryOperations(int operator_index, int symmetry_code);
            
            /**
             * Checks if the operations should be skipped.
             * If new spot given by the operator is not comparable then the 
             * operation should be skipped, this is returned by the this function
             * @return is the operation is to be skipped
             */
            bool SkipOperation() const;
            
            /**
             * Evaluates the corresponding symmetric index and changes them inplace.
             * Given a operator index, the new symmetric spots will have following values:
             *   Op  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 
             *   H= -h +h -h +k +k -k -k +h -h +k -k -h +h -h +h -h +h -h +k +k -k -k +h -h +k -k -h +h -h +h
             *                                       -k +k -k +k                                  -k +k -k +k
             *   K= +k -k -k +h -h +h -h -h +h -h +h +h -h +k -k +k -k -k +h -h +h -h -h +h -h +h +h -h +k -k
             *                           -k +k -k +k                                  -k +k -k +k            
             *   L= +l +l +l +l +l +l +l +l +l +l +l +l +l +l +l -l -l -l -l -l -l -l -l -l -l -l -l -l -l -l
             * @param h : the input and output h
             * @param k : the input and output k
             * @param l : the input and output l
             */
            void SymmetricMillerIndex(int* h, int* k, int* l) const;
            
            /**
             * Evaluates the corresponding phase change for the operator
             * @param phase : input phase
             * @param h : input h
             * @param k : input k
             * @param l : input l
             * @return the new phase
             */
            double PhaseChange(const double phase, const int h, const int k, const int l) const;
            
        private:
            
            /**
             * set the number of operations and symmetries
             */
            enum
            { 
                operations=30, symmetries=17
            };
            
            /**
             * Initializes the members given operator_index and symmetry code
             * @param operator_index
             * @param symmetry_code
             */
            void initialize(int operator_index, int symmetry_code);
            
            /**
             * Define how the h needs to be changed for each operation
             */
            const int hChanges[operations] = {-1,  1, -1, 2,  2, -2, -2,  1, -1,  2, -2, -3,  3, -3 , 3, -1,  1, -1,  2,  2, -2, -2,  1, -1,  2, -2, -3,  3, -3,  3};
            
            /**
             * Define how the k needs to be changed for each operation
             */
            const int kChanges[operations] = { 2, -2, -2, 1, -1,  1, -1, -3,  3, -3,  3,  1, -1,  2, -2,  2, -2, -2,  1, -1,  1, -1, -3,  3, -3,  3,  1, -1,  2, -2};
            
            /**
             * Define how the l needs to be changed for each operation
             */
            const int lChanges[operations] = { 1,  1,  1, 1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1};
            
            /**
             * Define how the phase changes for each operation with all the
             * symmetries
             */
            const int phaseChanges[symmetries][operations] 
            {
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4, 4, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1},
                {0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
                {0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 1, 1},
            };
            
            
            /**
             * The operator index of the instance
             */
            int index;
            
            /**
             * Corresponding code for change in h
             */
            int hChange;
            
            /**
             * Corresponding code for change in k
             */
            int kChange;
            
            /**
             * Corresponding code for change in l
             */
            int lChange;
            
            /**
             * Corresponding code for change in phase
             */
            int phaseChange;

        };// class SymmetryOperations
        
    } //namespace symmetrization
    
}//namespace volume_processing_2dx


#endif	/* SYMMETRY_OPERATIONS_HPP */


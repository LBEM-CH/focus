#ifndef PROJECTION_2D_HPP
#define PROJECTION_2D_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Projection2d;
	}
}

#include <fftw3.h>

#include <boost/scoped_ptr.hpp>

#include "../Typedefs.hpp"
#include "../DataStructures.hpp"


namespace SingleParticle2dx
{
	
	namespace DataStructures
	{
		
		/**
		 *  @brief     2d projection of 3d reference
		 *  @details   Class for 2d projections of the three reference. Data is stored as fourier data 
		 *             internally.
		 *  @author    Sebastian Scherer
		 *  @version   0.1
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class Projection2d : public SingleParticle2dx::DataStructures::Abstract2dData
		{
			
		public:

			/** 
			 *  @brief      Default constructor
			 *  @details    Calls the default constructor of all internal variables and set the fourier
			 *              component to a 1x1 array with value (0,0)
			 */
			Projection2d ();


			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  size_x Projection size x
			 *  @param[in]  size_y Projection size y
			 *  @post       Orientation set to (0,0,0), data array set to zero
			 */
			Projection2d (size_type size_x, size_type size_y);


			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor
			 *  @param[in]  size_x Projection size x
			 *  @param[in]  size_y Projection size x
			 *  @param[in]  o Orientation
			 *  @post       Orientation set to (0,0,0), data array set to zero
			 */
			Projection2d (size_type size_x, size_type size_y, SingleParticle2dx::DataStructures::Orientation o);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~Projection2d ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed Projection2d
			 *  @param[in]  rhs Projection2d to copy
			 */
			Projection2d (Projection2d const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed Projection2d
			 *  @param[in]  rhs Projection2d to assign
			 */
			Projection2d& operator= (Projection2d rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two Projection2ds by exchanging the pointers to the members of 
			 *              each Projection2d by means of std::swap. This technique enables the 
			 *              copy-and-swap trick.
			 *  @param[in]  p1 first Projection2d
			 *  @param[in]  p2 second Projection2d
			 */
			friend void swap (Projection2d& p1, Projection2d& p2);


			/**
			 *  @brief      ()-operator
			 *  @details    Random access operator returning a reference
			 *  @param[in]  i Index in first dimension
			 *  @param[in]  j Index in second dimension
			 *  @return     reference to data point at (i,j)
			 */
			fft_type& operator () (const size_type i, const size_type j);


			/**
			 *  @brief      Returns the orientation of the projection
			 *  @details    Trivial get-Operation
			 *  @return     Reference to the orientation of the projection
			 */
			SingleParticle2dx::DataStructures::Orientation& getOrientation();
			

			/**
			 *  @brief      Set orientation
			 *  @details    Trivial set-Operation making a deep copy of the passed orientation
			 *  @param[in]  o Orientation to set
			 */
			void setOrientation (SingleParticle2dx::DataStructures::Orientation o);
			
			
			void setMaskingMethod( value_type key );
			
			
			virtual void setupFromConfig();

		private:

			/** Orientation of the projection */
			SingleParticle2dx::DataStructures::Orientation m_orientation;

		};
		
	} /* projection2d */
	
} /* SingleParticle2dx */

#endif /* end of include guard: PROJECTION_2D_HPP */

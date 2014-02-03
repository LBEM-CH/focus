#ifndef SINGLE_PARTICLE_PARTICLE_SHIFT_HPP
#define SINGLE_PARTICLE_PARTICLE_SHIFT_HPP

namespace SingleParticle2dx
{	
	namespace DataStructures
	{
		class ParticleShift;
	}
}

#include <vector>

#include <boost/serialization/utility.hpp>


#include "../Typedefs.hpp"
#include "InterfaceToString.hpp"

namespace SingleParticle2dx
{
	
	namespace DataStructures
	{
		
		/**
		 *  @brief     In plane shift of one single particle 
		 *  @details   As a picked particle has not to be perfectly centred the Single Particle 
		 *             algorithm can change the center of each particle. This change is stored in form 
		 *             of a shift. The shift is given in number of pixels the current center 
		 *             distinguishes from the original center.
		 *  @author    Sebastian Scherer
		 *  @version   0.2
		 *  @date      2012
		 *  @copyright GNU Public License
		 */
		class ParticleShift : public SingleParticle2dx::DataStructures::InterfaceToString
		{
		
		public:

			/** Internal type of the shift in one dimension */
			typedef SingleParticle2dx::value_type value_type;


			/** Internal type of the particle shift in all two dimensions */
			typedef std::pair<value_type,value_type> value_array_type;


			/**
			 *  @brief      Default constructor
			 *  @details    Default constructor of ParticleShift which sets all the shift in all spacial
			 *              direction to zero
			 *  @post       shift set to zero
			 **/
			ParticleShift ();


			/**
			 *  @brief      Constructor
			 *  @details    Constructor of ParticleShift which sets the shift to a certain value given
			 *              as the constructor's arguments
			 *  @param[in]  x shift in x direction
			 *  @param[in]  y shift in y direction
			 *  @post       shift set to (x,y)
			 */
			ParticleShift (value_type x, value_type y);


			/**
			 *  @brief      Constuctor
			 *  @details    Constructor that takes a value_array_type object and sets the shift 
			 *              accordingly. The constructor makes a deep copy of passed variable rhs.
			 *  @param[in]  rhs shift value as value_array_type
			 *  @post       shift is set to rhs
			 *  @note       rhs has to be of value_array_type
			 */
			ParticleShift (value_array_type rhs);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~ParticleShift ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed ParticleShift
			 *  @param[in]  rhs ParticleShift to copy
			 */
			ParticleShift (ParticleShift const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed PartcleShift
			 *  @param[in]  rhs ParticleShift to assign
			 */
			ParticleShift& operator= (ParticleShift rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two PaticleShifts by exchanging the pointers to the members of 
			 *              each PartilceShift by means of std::swap. This technique enables the 
			 *              copy-and-swap trick.
			 *  @param[in]  s1 first ParticleShift
			 *  @param[in]  s2 second ParticleShift
			 */
			friend void swap (ParticleShift& s1, ParticleShift& s2);


			/**
			 *  @brief      Seturns the shift value in x direction
			 *  @details    Trivial get-operation
			 *  @return     The x-value of the shift
			 */
			value_type getShiftX () const;


			/**
			 *  @brief      Sets the shift value in x direction
			 *  @details    Trivial set-Operation
			 *  @param[in]  s value to set
			*/
			void setShiftX (const value_type s);


			/**
			 *  @brief      Returns the shift value in y direction
			 *  @details    Trivial get-operation
			 *  @return     The y-value of the shift
			 */
			value_type getShiftY() const;


			/**
			 *  @brief      Set the shift value in y direction
			 *  @details    Trivial set-operation
			 *  @param[in]  s value to set
			 */
			void setShiftY (const value_type s);
			
			
			void reset();
			
			
			virtual std::string getDataString();
			
			
			virtual std::string getDescString();

		private:

			/** @brief Data container storing the shift values */
			value_array_type m_data;
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_data;
			}

		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_PARTICLE_SHIFT_HPP */

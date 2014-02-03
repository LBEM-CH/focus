#ifndef SINGLE_PARTICLE_2DX_ORIENTATION_HPP
#define SINGLE_PARTICLE_2DX_ORIENTATION_HPP

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class Orientation;
	}
}

#include <vector>

#include <boost/serialization/utility.hpp>
#include <boost/serialization/vector.hpp>

#include <eigen3/Eigen/Dense>

#include "../Typedefs.hpp"
#include "InterfaceToString.hpp"

namespace SingleParticle2dx
{
	
	namespace DataStructures
	{
		
		/**
		 *  @brief     Class storing the orientation of a single 2d image of a particle
		 *  @details   As our Single Particle refinement determines the tilt geometry if each partcile 
		 *             individually, we have to store the individual orientation.
		 *             In order to avoid problems at zero tilt we store the normalized unit vector of
		 *             the individual image
		 *  @author    Sebastian Scherer
		 *  @version   0.2
		 *  @date      2012
		 *  @copyright GNU Public License
		 *  @todo      Change from angles to normals
		 */
		class Orientation : public SingleParticle2dx::DataStructures::InterfaceToString
		{
		public:

			/** Internal data type used for storing an angular value */
			typedef SingleParticle2dx::value_type value_type;


			/** Data type used for storing all three components describing the orientation of a particle */
			typedef std::vector<value_type> value_array_type;


			/** 
			 *  @brief      Default constructor
			 *  @details    Initializes the Orientation with zero.
			 *  @post       Components set to (0, 0, 0)
			 */
			Orientation ();


			/**
			 *  @brief      Constructor
			 *  @details    Costum constructor used to construct a angular geometry based on well known
			 *              angles
			 *  @param[in]  tltaxis Tilt axis
			 *  @param[in]  tltang Tilt angle
			 *  @param[in]  taxa Crystal orientation
			 *  @post       Content set to (taltaxis, tltang, taxa)
			 */
			Orientation (value_type tltaxis, value_type tltang, value_type taxa);

			/**
			 *  @brief      Consturctor
			 *  @details    Costum constructor that takes a value_array_type object and sets the components
			 *              accordingly. The constructor makes a deep copy of passed variable rhs.
			 *  @param[in]  rhs Component values as value_array_type
			 *  @post       Components set to rhs
			 *  @note       rhs has to be of value_array_type
			 */
			Orientation (value_array_type rhs);


			/**
			 *  @brief      Destructor
			 *  @details    All the allocated memory is freed afer calling the Destructor
			 */
			virtual ~Orientation ();


			/**
			 *  @brief      Copy-Constructor
			 *  @details    Makes a deep copy of the passed Orientation
			 *  @param[in]  rhs Orientation to copy
			 */
			Orientation (Orientation const& rhs);


			/**
			 *  @brief      Assignment operator
			 *  @details    Invokes a deep copy of the passed Orientation
			 *  @param[in]  rhs Orientation to assign
			 */
			Orientation& operator= (Orientation rhs);
			
			
			/**
			 *  @brief      operator==
			 *  @details    
			 *  @return     true if input equals *this, else false
			 *  @param[in]  rhs Orientation to compare
			 */
			bool operator== (Orientation rhs);


			/**
			 *  @brief      Friend swap function
			 *  @details    Exchanges two Orientations by exchanging the pointers to the members of each
			 *              Orientation by means of std::swap. This technique enables the copy-and-swap
			 *              trick.
			 *  @param[in]  o1 first Orientation
			 *  @param[in]  o2 second Orientation
			 */
			friend void swap (Orientation& o1, Orientation& o2);


			/**
			 *  @brief      Sets the orientation to a certain value
			 *  @details    Trivial set-operation
			 *  @param[in]  rhs Value to set
			 */
			void setOrientation(value_array_type rhs);
			
			
			/**
			 *  @brief      Sets the orientation to a certain value
			 *  @details    Trivial set-operation
			 *  @param[in]  rhs Value to set
			 */
			void setOrientation(Eigen::Vector3f rhs);


			/**
			 *  @brief      Return the orientation as value_array_type
			 *  @details    Trivial get-operation
			 *  @return     Stored orientation
			 */
			value_array_type getOrientation();


			/**
			 *  @brief      Returns the first component
			 *  @details    Trivial get-operation
			 *  @return     Tilt Axis
			 */
			value_type getTLTAXIS () const;


			/**
			 *  @brief      Sets the first component
			 *  @details    Trivial set-operation
			 *  @param[in]  tltaxis
			 */
			void setTLTAXIS (const value_type tltaxis);


			/**
			 *  @brief      Returns the second component
			 *  @details    Trivial get-operation
			 *  @return     Tilt Angle
			 */
			value_type getTLTANG () const;


			/**
			 *  @brief      Sets the second component
			 *  @details    Trivial set-operation
			 *  @param[in]  tltang
			 */
			void setTLTANG (const value_type tltang);


			/** 
			 *  @brief      Returns the third component
			 *  @details    Trivial get-operation
			 *  @return     TAXA
			 */
			value_type getTAXA () const;


			/**
			 *  @brief      Sets the third component
			 *  @details    Trivial set-operation
			 *  @param[in]  taxa Crystal Orientation
			 */
			void setTAXA (const value_type taxa);
			
			std::string getDataString();
			std::string getDescString();
			
			friend bool operator< (Orientation o1, Orientation o2)
			{
				if( o1.getTLTAXIS() != o2.getTLTAXIS() )
				{
					return o1.getTLTAXIS() < o2.getTLTAXIS();
				}
				if ( o1.getTLTANG() != o2.getTLTANG() )
				{
					return o1.getTLTANG() < o2.getTLTANG();
				}
				return o1.getTAXA() < o2.getTAXA();
			}

		protected:
			
			/**
			 *  @brief      Stores valid values for the orientation
			 *  @details    All values set to legal values
			 */
			void normalize();

		private:

			/** @brief Data container storing the angular values */
			value_array_type m_angles;
			
			friend class boost::serialization::access;
			   
			template<class Archive>
			void serialize(Archive & ar, const unsigned int version)
			{
				ar & m_angles;
			}

		};
		
	} /* DataStructures */
	
} /* SingleParticle2dx */

#endif /* end of include guard: SINGLE_PARTICLE_2DX_ORIENTATION_HPP */

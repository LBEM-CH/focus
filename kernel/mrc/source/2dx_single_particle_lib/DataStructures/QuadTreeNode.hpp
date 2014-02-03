#ifndef QUADTREENODE_HPP_AB6V2M3D
#define QUADTREENODE_HPP_AB6V2M3D

namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class QuadTreeNode;
	}
}


#include "../Typedefs.hpp"
#include "../Methods.hpp"
#include "../Utilities.hpp"
#include "../DataStructures.hpp"


namespace SingleParticle2dx
{
	namespace DataStructures
	{
		class QuadTreeNode
		{
		public:
			QuadTreeNode(value_type width, value_type center_x, value_type center_y, size_type level);
			
			
			QuadTreeNode(value_type width, value_type center_x, value_type center_y, SingleParticle2dx::DataStructures::ParticleContainer& cont);
		
		
			~QuadTreeNode();
		
			
			void split();
		
		
			size_type getDepth();
		
		
			void plot();
		
		
			void extractLayer(size_type level, SingleParticle2dx::DataStructures::ParticleContainer& cont, value_type n, size_type image_number, SingleParticle2dx::DataStructures::Reconstruction3d& ref);
		
		
			void updateOrientations(size_type level, SingleParticle2dx::DataStructures::ParticleContainer& cont, size_type& index);
		
		
			void prepareLayer(size_type level, SingleParticle2dx::DataStructures::Reconstruction3d& ref);
			
		private:
		
			value_type m_width;
		
			value_type m_center_x;
		
			value_type m_center_y;
		
			size_type m_max_per_node;
		
			size_type m_level;
		
			bool m_used;
			
			std::vector<QuadTreeNode*> m_kids;
		
			std::vector<SingleParticle2dx::DataStructures::Particle*> m_data;
		
		};
	}
}

#endif /* end of include guard: QUADTREENODE_HPP_AB6V2M3D */

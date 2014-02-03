#include "QuadTreeNode.hpp"



SingleParticle2dx::DataStructures::QuadTreeNode::QuadTreeNode(value_type width, value_type center_x, value_type center_y, size_type level)
 : m_width(width),
   m_center_x(center_x),
   m_center_y(center_y),
   m_max_per_node(10),
   m_level(level),
   m_used(true)
{}


SingleParticle2dx::DataStructures::QuadTreeNode::QuadTreeNode(value_type width, value_type center_x, value_type center_y, SingleParticle2dx::DataStructures::ParticleContainer& cont)
 : m_width(width),
   m_center_x(center_x),
   m_center_y(center_y),
   m_max_per_node(10),
   m_level(1),
   m_used(true)
{
	for(size_type i=0; i<cont.getNumberOfParticles(); i++)
	{
		m_data.push_back(&cont(i));
	}
}


SingleParticle2dx::DataStructures::QuadTreeNode::~QuadTreeNode()
{
	if (m_kids.size()>0)
	{
		for(int i=0; i<4; i++)
		{
			delete m_kids[i];
		}
	}
}


SingleParticle2dx::size_type SingleParticle2dx::DataStructures::QuadTreeNode::getDepth()
{
	std::vector<size_type> aux;
	if (m_kids.size() < 1)
	{
		return 0;
	}
	else
	{
		for (size_type i=0; i<4; i++)
		{
			aux.push_back(m_kids[i]->getDepth()+1);
		}
	}
	return *(std::max_element(aux.begin(), aux.end()));
}


void SingleParticle2dx::DataStructures::QuadTreeNode::plot()
{
	for(size_type i=0; i<m_level; i++)
	{
		std::cout << "\t";
	}
	std::cout << m_data.size() << std::endl;
	
	if (m_kids.size()<4)
	{
		return;
	}
	else
	{
		for(int i=0; i<4; i++)
		{
			m_kids[i]->plot();
		}
	}
}


void SingleParticle2dx::DataStructures::QuadTreeNode::prepareLayer(size_type level, SingleParticle2dx::DataStructures::Reconstruction3d& ref)
{
	if ( (m_level == (level-1)) && (level>1) && (m_data.size()>0) && m_used)
	{
		for(int i=0; i<4; i++)
		{
			for(size_type j=0; j<static_cast<size_type>(m_kids[i]->m_data.size()); j++ )
			{
				m_kids[i]->m_data[j]->setOrientation(m_data[0]->getNewOrientation());
			}
		}
		
		SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		SingleParticle2dx::DataStructures::Projection2d p(config->getParticleSize(), config->getParticleSize());
		SingleParticle2dx::DataStructures::ParticleContainer dummy_cont;
		dummy_cont.addParticle(*m_data[0]);
		
		ref.setMinimal(false);
		ref.setProjectionMethod(2);
		ref.forceProjectionPreparation(dummy_cont);
		ref.calculateProjection(m_data[0]->getNewOrientation(), p);
	
		for(int i=0; i<4; i++)
		{
			/** @TODO */
			SingleParticle2dx::Utilities::UtilityFunctions::alignInplaneShift(m_kids[i]->m_data, p, true, config->getCCDiscard(), false);
		}
		
		return;
	}
	
	if (m_kids.size() > 0)
	{
		for(int i=0; i<4; i++)
		{
			m_kids[i]->prepareLayer(level, ref);
		}
	}
}


void SingleParticle2dx::DataStructures::QuadTreeNode::updateOrientations(size_type level, SingleParticle2dx::DataStructures::ParticleContainer& cont, size_type& index)
{
	if ( m_level>level )
	{
		return;
	}
	
	if ( m_data.size() < 1 )
	{
		return;
	}
	
	if ( (m_level==level) && (m_data.size()>0) && m_used)
	{
		for(size_type i=0; i<static_cast<size_type>(m_data.size()); i++)
		{
			m_data[i]->setOrientation(cont(index).getNewOrientation());
			m_data[i]->setSimMeasure(cont(index).getSimMeasure());
			m_data[i]->setWeight(cont(index).getWeight());
		}
		index++;
	}
	
	if (m_kids.size() > 0)
	{
		for(size_type i=0; i<4; i++)
		{
			m_kids[i]->updateOrientations(level, cont, index);
		}
	}
}


void SingleParticle2dx::DataStructures::QuadTreeNode::extractLayer(size_type level, SingleParticle2dx::DataStructures::ParticleContainer& cont, value_type n, size_type image_number, SingleParticle2dx::DataStructures::Reconstruction3d& ref)
{
	if ( m_level>level )
	{
		return;
	}
	
	if ( m_data.size() < 1 )
	{
		return;
	}
	
	if ( (m_level==level) && (m_data.size()>0) && m_used)
	{
		SingleParticle2dx::real_array2d_type ave_im ( boost::extents[m_data[0]->getSizeX()][m_data[0]->getSizeY()] );
		SingleParticle2dx::real_array2d_type tmp_im ( boost::extents[m_data[0]->getSizeX()][m_data[0]->getSizeY()] );
		
		SingleParticle2dx::DataStructures::Orientation o = m_data[0]->getInitialOrientation();
		SingleParticle2dx::DataStructures::GlobalParticleInformation info( 1, m_center_x, m_center_y );
		SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
		SingleParticle2dx::DataStructures::Particle part(config->getParticleSize(), config->getParticleSize(), o, info);
		
		for(size_type i=0; i<static_cast<size_type>(m_data.size()); i++)
		{
			if (m_data[i]->getRejectTreeAlignment())
			{
				std::cout << "rejecting because of tree-alignment" << std::endl;
				continue;
			}
			
			part = (*m_data[i]);
			part.updateParticleShift();
			part.getRealSpaceData(&tmp_im);
			SingleParticle2dx::Utilities::DataContainerFunctions::addRealData(&tmp_im, 1.0/static_cast<value_type>(m_data.size()), &ave_im);
		}
		
		SingleParticle2dx::DataStructures::Particle p(m_data[0]->getSizeX(), m_data[0]->getSizeY(), o, info);
		p.setFourierSpaceData(&ave_im);
		
		SingleParticle2dx::DataStructures::Reconstruction3d rec_local = ref;
		SingleParticle2dx::DataStructures::ParticleContainer cont_local;
		cont_local.addParticle(p);
		rec_local.setMinimal(false);
		rec_local.setProjectionMethod(2);
		rec_local.updateReconstruction(cont_local, false, false, true);
		value_type local_cc = cont_local(0).getSimMeasure();
		p.setSimMeasure(local_cc);
		
		//p.setWeight(powf(static_cast<value_type>(m_data.size())/static_cast<value_type>(n),2));
		p.setWeight(static_cast<value_type>(m_data.size())/static_cast<value_type>(n));
		//p.setWeight(1);
		
		p.getGlobalParticleInformation().setImageNumber(image_number);
		cont.addParticle(p);
	}
	
	if (m_kids.size() > 0)
	{
		for(size_type i=0; i<4; i++)
		{
			m_kids[i]->extractLayer(level, cont, n, image_number, ref);
		}
	}
}


void SingleParticle2dx::DataStructures::QuadTreeNode::split()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	std::cout << "current particle of node: " << static_cast<size_type>(m_data.size()) << std::endl;
	//if ( static_cast<size_type>(m_data.size()) > m_max_per_node )
	if ( m_level < config->getTreeLevel() )
	{
		m_kids.push_back(new QuadTreeNode(m_width/2, m_center_x-m_width/4, m_center_y-m_width/4, m_level+1));
		m_kids.push_back(new QuadTreeNode(m_width/2, m_center_x-m_width/4, m_center_y+m_width/4, m_level+1));
		m_kids.push_back(new QuadTreeNode(m_width/2, m_center_x+m_width/4, m_center_y-m_width/4, m_level+1));
		m_kids.push_back(new QuadTreeNode(m_width/2, m_center_x+m_width/4, m_center_y+m_width/4, m_level+1));
		
		SingleParticle2dx::DataStructures::GlobalParticleInformation info;
		
		for(size_type i=0; i<static_cast<size_type>(m_data.size()); i++)
		{
			info = (*m_data[i]).getGlobalParticleInformation();
			if ( info.getPositionX() < m_center_x )
			{
				if (info.getPositionY() < m_center_y )
				{
					// SW
					m_kids[0]->m_data.push_back(m_data[i]);
				}
				else
				{
					// NW
					m_kids[1]->m_data.push_back(m_data[i]);
				}
			}
			else
			{
				if (info.getPositionY() < m_center_y )
				{
					// SE
					m_kids[2]->m_data.push_back(m_data[i]);
				}
				else
				{
					// SW
					m_kids[3]->m_data.push_back(m_data[i]);
				}
			}
		}
		
		value_type parent_density = SingleParticle2dx::Utilities::UtilityFunctions::calculateDensity(m_data, m_width);
		
		for(int i=0; i<4; i++)
		{
			if ( (SingleParticle2dx::Utilities::UtilityFunctions::calculateDensity( m_kids[i]->m_data, m_kids[i]->m_width)<0.90*parent_density) && (m_kids[i]->m_level>2) )
			{
				m_kids[i]->m_used = false;
				SingleParticle2dx::Utilities::UtilityFunctions::disableParticles(m_kids[i]->m_data);
			}	
			m_kids[i]->split();
		}
	}
}


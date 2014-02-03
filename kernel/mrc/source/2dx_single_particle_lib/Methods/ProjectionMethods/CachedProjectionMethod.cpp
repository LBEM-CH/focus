#include "CachedProjectionMethod.hpp"

#include "../../Config.hpp"


#include <parallel/algorithm>


#include <omp.h>


SingleParticle2dx::Methods::CachedProjectionMethod::CachedProjectionMethod (SingleParticle2dx::DataStructures::Reconstruction3d* context)
{
	m_context = context;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	setTrialAngleGenerator( config->getTrialAngleGenerator() );
	
	if ( config->getParallelProjection() )
	{
		m_projector.reset( new SingleParticle2dx::Methods::ThreadSafeProjectionMethod( m_context ) );
	}
	else
	{
		switch (static_cast<int>(config->getProjectionMethod()))
		{
			case 0:
				m_projector.reset( new SingleParticle2dx::Methods::RealSpaceProjectionMethod( m_context ) );
				break;
			case 1:
				m_projector.reset( new SingleParticle2dx::Methods::FourierSpaceProjectionMethod( m_context ) );
				break;
			case 2:
				m_projector.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( m_context ) );
				break;
			case 3:
				m_projector.reset( new SingleParticle2dx::Methods::EMANFourierGriddingProjector( m_context ) ); 
				break;
			#ifdef USE_CUDA
			case 4:
				m_projector.reset( new SingleParticle2dx::Methods::CUDAProjectionMethod( m_context ) ); 
				break;
			#endif
			default:
				std::cerr << "unknown projection method in cached proj" << std::endl;
				m_projector.reset( new SingleParticle2dx::Methods::EMANProjectionMethod( m_context ) );
				// std::runtime_error("Bad operation");
		}
	}
}


SingleParticle2dx::Methods::CachedProjectionMethod::~CachedProjectionMethod ()
{
	clearProjections();
}


void SingleParticle2dx::Methods::CachedProjectionMethod::clearProjections()
{
	std::cout << "::Proj clean called" << std::endl;
	proj_map.clear();
}


void SingleParticle2dx::Methods::CachedProjectionMethod::prepareForProjections(SingleParticle2dx::DataStructures::ParticleContainer& cont)
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	setTrialAngleGenerator(config->getTrialAngleGenerator());

	std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
	cont.getDistinctAngles(o_vec);
	
	proj_map.clear();
		
	std::vector<SingleParticle2dx::DataStructures::Orientation> o_proj_vec;
	
	std::cout << "size of dist vector " << o_vec.size() << std::endl;
	
	for (size_type i=0; i<static_cast<size_type>(o_vec.size()); i++)
	{
		#pragma omp critical (eman_const_call_trial_angles)
		{
			m_angle_generator->generateAngles(o_vec[i], o_proj_vec );
		}
	}
	
	std::cout << ":trial sort started" << std::endl;
	
	//std::sort(o_proj_vec.begin(), o_proj_vec.end());
	__gnu_parallel::sort(o_proj_vec.begin(), o_proj_vec.end());
	
	std::cout << ":trial sort done" << std::endl;
    o_proj_vec.erase(std::unique(o_proj_vec.begin(), o_proj_vec.end()), o_proj_vec.end());
	
	std::cout << "::" << o_proj_vec.size() << " trial projection will be generated" << std::endl;
		
	SingleParticle2dx::DataStructures::ParticleContainer dummy_cont;
	m_projector.get()->prepareForProjections(dummy_cont);
	
	size_type n = config->getParticleSize();
	size_type counter = 0;
	
	SingleParticle2dx::Utilities::SystemTimer timer;
	float t_start = timer.GetTime();

	if ( config->getParallelProjection() )
	{
		#pragma omp parallel for
		for (size_type i=0; i<static_cast<size_type>(o_proj_vec.size()); i++)
		{
			SingleParticle2dx::DataStructures::Projection2d new_proj(n,n);
			m_projector.get()->calculateProjection(o_proj_vec[i], new_proj);

			#pragma omp critical (insert_proj)
			{
				if(counter%100 == 0)
				{
					std::cout << counter << " of " << o_proj_vec.size() << std::endl;
				}
				
				//std::cout << "calculating proj with " << o_proj_vec[i].getTLTAXIS() << " " << o_proj_vec[i].getTLTANG() << " " << o_proj_vec[i].getTAXA() << std::endl;
				proj_map[o_proj_vec[i]] = new_proj; 
				counter++;
			}
		}
	}
	else
	{
		for (size_type i=0; i<static_cast<size_type>(o_proj_vec.size()); i++)
		{
			SingleParticle2dx::DataStructures::Projection2d new_proj(n,n);
			m_projector.get()->calculateProjection(o_proj_vec[i], new_proj);
			{
				//std::cout << "calculating proj with " << o_proj_vec[i].getTLTAXIS() << " " << o_proj_vec[i].getTLTANG() << " " << o_proj_vec[i].getTAXA() << std::endl;
				proj_map[o_proj_vec[i]] = new_proj; 
			}
			
			if(counter%100 == 0)
			{
				std::cout << counter << " of " << o_proj_vec.size() << std::endl;
			}
			counter++;
		}
	}
	
	if(counter != static_cast<size_type>(proj_map.size()))
	{
		std::cerr << "Wrong number of projections cached" << std::endl;
		throw std::runtime_error("Bad operation");
	}
	
	float t_used = timer.GetTime() - t_start;
	std::cout << "::time for proj: " << t_used << std::endl;

	std::cout << "cached projections done" << std::endl;
	
}


void SingleParticle2dx::Methods::CachedProjectionMethod::calculateProjection(SingleParticle2dx::DataStructures::Orientation& o, SingleParticle2dx::DataStructures::Projection2d& p)
{
	//std::cout << "asked for proj " << o.getTLTAXIS() << " " << o.getTLTANG() << " " << o.getTAXA() << std::endl;
	
	//std::cout << "map_size: " << proj_map.size() << std::endl;
	p = proj_map[o];
	
	//std::cout << "getting proj from cached container " << p.getOrientation().getTLTAXIS() << " " << p.getOrientation().getTLTANG() << " " << p.getOrientation().getTAXA() << std::endl;
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	size_type n = config->getParticleSize();
	
	if (p.getSizeX() != n)
	{
		std::cerr << "something went wrong while caching the projection: " << o.getTLTAXIS() << " " << o.getTLTANG() << " " << o.getTAXA() << std::endl;
		throw std::runtime_error("Bad operation");
	}
}

#ifndef INTERFACEHASTRIALANGLEGENERATOR_HPP_JZQKRT3Y
#define INTERFACEHASTRIALANGLEGENERATOR_HPP_JZQKRT3Y

namespace SingleParticle2dx
{	
	namespace Methods
	{		
		class InterfaceHasTrialAngleGenerator;
	}
}

#include "../TrialAngleGenerators.hpp"


namespace SingleParticle2dx
{
	
	namespace Methods
	{
		
		class InterfaceHasTrialAngleGenerator
		{
						
		protected:
			
			void setTrialAngleGenerator( size_type key )
			{
				#pragma omp critical (reset_ang_gen)
				{
					switch (static_cast<int>(key))
					{
						case 0:
							m_angle_generator.reset( new SingleParticle2dx::Methods::UniformTrialAngleGenerator );
							break;
						case 1:
							m_angle_generator.reset( new SingleParticle2dx::Methods::RotTrialAngleGenerator );
							break;
						case 2:
							m_angle_generator.reset( new SingleParticle2dx::Methods::InPlaneTrialAngleGenerator );
							break;
						case 3:
							m_angle_generator.reset( new SingleParticle2dx::Methods::EMANTrialAngleGenerator );
							break;
						case 4:
							m_angle_generator.reset( new SingleParticle2dx::Methods::DummyTrialAngleGenerator );
							break;
						case 5:
							m_angle_generator.reset( new SingleParticle2dx::Methods::ConeTrialAngleGenerator );
							break;
						case 6:
							m_angle_generator.reset( new SingleParticle2dx::Methods::FullTrialAngleGenerator );
							break;
						case 7:
							m_angle_generator.reset( new SingleParticle2dx::Methods::FullFullTrialAngleGenerator );
							break;
						case 8:
							m_angle_generator.reset( new SingleParticle2dx::Methods::ModFullTrialAngleGenerator );
							break;
						case 9:
							m_angle_generator.reset( new SingleParticle2dx::Methods::FixAxisTrialAngleGenerator );
							break;	
						case 10:
							m_angle_generator.reset( new SingleParticle2dx::Methods::TiltAxisTrialAngleGenerator );
							break;
						case 11:
							m_angle_generator.reset( new SingleParticle2dx::Methods::IsoTrialAngleGenerator );
							break;
						default:
							std::cerr << "unknown trial angle generator" << std::endl;
							m_angle_generator.reset( new SingleParticle2dx::Methods::DummyTrialAngleGenerator );
							throw std::runtime_error("Bad operation");
					}
				}
			}
			
			/** sample angle generator */
			boost::scoped_ptr<SingleParticle2dx::Methods::AbstractTrialAngleGenerator> m_angle_generator;
			
		};
		
	} /* Methods */
	
} /* SingleParticle2dx */


#endif /* end of include guard: INTERFACEHASTRIALANGLEGENERATOR_HPP_JZQKRT3Y */

#include "../2dxSingleParticle.hpp"


int main()
{
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	boost::scoped_ptr<SingleParticle2dx::Methods::AbstractTrialAngleGenerator> m_angle_generator;
	
	switch (static_cast<int>(config->getTrialAngleGenerator()))
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
	
	std::vector<SingleParticle2dx::DataStructures::Orientation> o_vec;
	m_angle_generator->generateAngles(SingleParticle2dx::DataStructures::Orientation(0,0,0), o_vec );

	std::string filename = "angle_file.txt";
	boost::shared_ptr<FILE> file( fopen ( filename.c_str(), "w" ), SingleParticle2dx::Utilities::CustomizedDeleter() );
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	
	for(int i=0; i<static_cast<int>(o_vec.size()); i++)
	{
		fprintf(file.get(), "%f\t%f\t%f\n", o_vec[i].getTLTAXIS(), o_vec[i].getTLTANG(), o_vec[i].getTAXA());
		
		SingleParticle2dx::DataStructures::Particle part(10,10);
		part.setOrientation(o_vec[i]);
		cont.addParticle(part);
	}
	
	cont.writeStatToFile("angle_dummy_stat.txt");

	return 0;
}

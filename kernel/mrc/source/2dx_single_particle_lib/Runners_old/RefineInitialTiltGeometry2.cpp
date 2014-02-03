#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main ()
{

	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	
	
	SingleParticle2dx::ConfigContainer* config = SingleParticle2dx::ConfigContainer::Instance();
	
	int n;
	
	std::string cont_path = config->getContainerFilePath();
	
	SingleParticle2dx::DataStructures::ParticleContainer cont;
	
	SingleParticle2dx::Utilities::UtilityFunctions::setProgressBar( 10 );
	
	std::string cont_bin_file = cont_path + "/ParticleContainers/shift_corrected.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::deserializeContainerFromDisk(cont_bin_file, cont, true);

	cont.setParticleNumbers();
	
	n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
	
	std::cout << cont.getNumberOfParticles() << std::endl;
	std::vector<SingleParticle2dx::DataStructures::ParticleContainer> cont_vec;
	
	SingleParticle2dx::DataStructures::Reconstruction3d rec3d_ave(n, n, n);
	SingleParticle2dx::DataStructures::ParticleContainer cont_ave;
	cont.generateAverageContainer(cont_ave, true);
	rec3d_ave.generateInitialModel(cont_ave);
	rec3d_ave.applyMask();
	rec3d_ave.applyLowPassFilter();
	
	rec3d_ave.writeToFile( "REC_SP/rec_initial_before_refine2.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("REC_SP/rec_initial_before_refine2.map", "MAP: Initial 3D reconstruction", config->getScriptName(), true);
	
	SingleParticle2dx::Utilities::ConvAnalyzer conv_analyzer(config->getMinAngleChange(), 3);
	
	
	for (int i=0; i<config->getMaxIteration(); i++)
	{
		rec3d_ave.updateReconstruction(cont_ave, false);
		rec3d_ave.applyMask();
		rec3d_ave.applyLowPassFilter();
		
		SingleParticle2dx::Utilities::UtilityFunctions::generateVariableOutput("angular_change_" + boost::lexical_cast<std::string>(i), rec3d_ave.getLastAngleChange(), config->getScriptName());
		
		conv_analyzer.addChange(rec3d_ave.getLastAngleChange());

		if (config->getKeepAll())
		{
			rec3d_ave.writeToFile( "REC_SP/rec_initial2_refine_" + boost::lexical_cast<std::string>(i) + ".map" );
			SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("REC_SP/rec_initial2_refine_" + boost::lexical_cast<std::string>(i) + ".map", "MAP: refine " + boost::lexical_cast<std::string>(i), config->getScriptName(), false);
		}

		if ( conv_analyzer.isConverged() )
		{
			SingleParticle2dx::Utilities::UtilityFunctions::generate2dxOutput("CONVERGERD, increase search range", 1);
			break;
		}
	}
	
	rec3d_ave.writeToFile( "REC_SP/rec_initial_after_refine2.map" );
	SingleParticle2dx::Utilities::UtilityFunctions::generateImageOutput("REC_SP/rec_initial_after_refine2.map", "MAP: Refined1 Iniital 3D reconstruction", config->getScriptName(), true);
	
	cont.updateInitialTiltGeometryAndShift(cont_ave);
	
	cont.writeStatToFile(cont_path + "/ContainerStatsTXT/cont_stat_initref2.txt");
	
	cont_bin_file = cont_path + "/FingerPrintContainers/initial_geometry_corrected2.bin";
	SingleParticle2dx::DataStructures::ParticleContainer::serializeContainerToDisk(cont, false, cont_bin_file);
	
	SingleParticle2dx::Utilities::UtilityFunctions::writeAveContainerStatToFile(cont_ave, cont_path + "/ContainerStatsTXT/picking_stat_refined2.txt");

}

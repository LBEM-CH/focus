#include "../2dxSingleParticle.hpp"

#include <fstream>

#include <boost/lexical_cast.hpp>

#include <boost/archive/binary_oarchive.hpp>
#include <boost/archive/binary_iarchive.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/archive/text_iarchive.hpp>
#include <boost/filesystem.hpp>
#include <boost/program_options.hpp>


int main (int argc, char const *argv[])
{
	namespace po = boost::program_options;
	
	typedef boost::archive::binary_iarchive archive_in_type;
	typedef boost::archive::binary_oarchive archive_out_type;
	
	std::string rec_in_file;
	std::string rec_out_file;
	std::string fsc_file;
	float factor;
	float resolution;
	
	po::options_description desc("Allowed options");
	desc.add_options()
	    ("help", "produce help message")
	    ("rec_in", po::value<std::string>(&rec_in_file), "Original reconstruction file")
		("rec_out", po::value<std::string>(&rec_out_file), "New reconstruction file")
		("fsc", po::value<std::string>(&fsc_file), "FSC")
		("factor", po::value<float>(&factor), "Factor")
		("res", po::value<float>(&resolution), "Resolution")
	;
	
	po::variables_map vm;
	po::store(po::parse_command_line(argc, argv, desc), vm);
	po::notify(vm);    

	if (vm.count("help")) {
	    cout << desc << "\n";
	    return 1;
	}
	
	if ( vm.count("rec_in") && vm.count("rec_out") && vm.count("fsc") && vm.count("factor") )
	{
		int n = SingleParticle2dx::ConfigContainer::Instance()->getParticleSize();
		SingleParticle2dx::DataStructures::Reconstruction3d rec3d(n, n, n);
		
		SingleParticle2dx::real_array3d_type in_data = SingleParticle2dx::real_array3d_type(boost::extents[n][n][n]);
		
		SingleParticle2dx::Utilities::MRCFileIO::readFromMrc(rec_in_file, &in_data);
		rec3d.setFourierSpaceData(in_data);
		
		std::pair<std::vector<float>, std::vector<float> > fsc_info;
		
		{
			std::ifstream ifs(fsc_file.c_str());
			archive_in_type ia(ifs);
			ia >> fsc_info;
		}
		
		rec3d.applyNegativeBFactor(fsc_info.second, resolution, factor);
		rec3d.writeToFile(rec_out_file);
		
	}
	else
	{
		cout << desc << "\n";
	    return 1;
	}
	
	return 0;
}

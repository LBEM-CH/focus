#include "quality_evaluation.hpp"


void tdx::utilities::quality_evaluation::corrected_phase_residual
                            (const tdx::data::MillerToPeakMultiMap peak_multimap, double a, double b, double c, double gamma, tdx::data::ResolutionBinnedData& binnedPR)
{
    tdx::data::ResolutionBinnedData binned_numerator_sums(binnedPR.min_resolution(), binnedPR.max_resolution(), binnedPR.resolution_bins());
    tdx::data::ResolutionBinnedData binned_denominator_sums(binnedPR.min_resolution(), binnedPR.max_resolution(), binnedPR.resolution_bins());
    
    tdx::data::MillerToPeakMap peak_map;
    tdx::utilities::fourier_utilities::average_peaks(peak_multimap, peak_map);
    
    for(tdx::data::MillerToPeakMultiMap::const_iterator spot_itr=peak_multimap.begin(); spot_itr!=peak_multimap.end(); ++spot_itr)
    {
        //The averaged value
        double averaged_amplitude = peak_map.at((*spot_itr).first).amplitude();
        double averaged_phase = peak_map.at((*spot_itr).first).phase();
        
        //The local value
        double phase = (*spot_itr).second.phase();
        double amplitude = (*spot_itr).second.amplitude();
        
        double phase_diff = tdx::utilities::angle_utilities::CorrectRadianPhase(averaged_phase-phase);
        double resolution = tdx::utilities::fourier_utilities::get_resolution((*spot_itr).first, gamma, a, b, c);

        //Update the sums in the bins
        binned_denominator_sums.add_data_at(1/resolution, std::abs(averaged_amplitude) * std::abs(amplitude));
        binned_numerator_sums.add_data_at(1/resolution, std::abs(averaged_amplitude) * std::abs(amplitude) * pow(phase_diff, 2));
    }
    
    //Divide numerator and denominator in each bin to get final binned phase residuals
    for(int bin=0; bin<binnedPR.resolution_bins(); bin++)
    {
        if(binned_denominator_sums.sum_in(bin) > 0.0000001)
        {
            double bin_phase_residual = sqrt(binned_numerator_sums.sum_in(bin)/binned_denominator_sums.sum_in(bin))*180/M_PI;
            binnedPR.set_bin_sum(bin, bin_phase_residual);
            binnedPR.set_bin_count(bin, 1); //Just to get correct averages
        }
    }
    
}

void tdx::utilities::quality_evaluation::fourier_shell_correlation
                            (const tdx::data::MillerToPeakMultiMap peak_multimap, double a, double b, double c, double gamma, tdx::data::ResolutionBinnedData& binnedFSC)
{
    tdx::data::ResolutionBinnedData binned_numerator_sums(binnedFSC.min_resolution(), binnedFSC.max_resolution(), binnedFSC.resolution_bins());
    tdx::data::ResolutionBinnedData binned_amp1_sums(binnedFSC.min_resolution(), binnedFSC.max_resolution(), binnedFSC.resolution_bins());
    tdx::data::ResolutionBinnedData binned_amp2_sums(binnedFSC.min_resolution(), binnedFSC.max_resolution(), binnedFSC.resolution_bins());
    
    tdx::data::MillerToPeakMap peak_map;
    tdx::utilities::fourier_utilities::average_peaks(peak_multimap, peak_map);
    
    for(tdx::data::MillerToPeakMultiMap::const_iterator spot_itr=peak_multimap.begin(); spot_itr!=peak_multimap.end(); ++spot_itr)
    {
        //The averaged value
        double averaged_amplitude = peak_map.at((*spot_itr).first).amplitude();
        double averaged_phase = peak_map.at((*spot_itr).first).phase();
        
        //The local value
        double phase = (*spot_itr).second.phase();
        double amplitude = (*spot_itr).second.amplitude();
        
        double phase_diff = tdx::utilities::angle_utilities::CorrectRadianPhase(averaged_phase-phase);
        double resolution = tdx::utilities::fourier_utilities::get_resolution((*spot_itr).first, gamma, a, b, c);

        //Update the sums in the bins
        binned_amp1_sums.add_data_at(1/resolution, averaged_amplitude * averaged_amplitude);
        binned_amp2_sums.add_data_at(1/resolution, amplitude * amplitude);
        binned_numerator_sums.add_data_at(1/resolution, std::abs(averaged_amplitude) * std::abs(amplitude) * cos(phase_diff));
    }
    
    //Calculate the FSC using the formula:
    // fsc = sum(amp1*amp2*cos(phase_diff)/sqrt(sum(amp1^2)*sum(amp2^2))
    for(int bin=0; bin<binnedFSC.resolution_bins(); bin++)
    {
        double denominator = sqrt(binned_amp1_sums.sum_in(bin)*binned_amp2_sums.sum_in(bin));
        if( denominator > 0.0000001)
        {
            double bin_fsc = binned_numerator_sums.sum_in(bin)/denominator;
            binnedFSC.set_bin_sum(bin, bin_fsc);
            binnedFSC.set_bin_count(bin, 1); //Just to get correct averages
        }
    }
    
}
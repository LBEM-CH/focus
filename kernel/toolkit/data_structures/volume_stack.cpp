#include <ctime>

#include "volume_stack.hpp"

using namespace tdx::data;

VolumeStack::VolumeStack(const std::string& filename, int start, int end) 
{
    Volume2DX volume3D;
    volume3D.read_volume(filename, "mrc");
    from_3D_volume(volume3D, start, end);
}

VolumeStack::VolumeStack(Volume2DX volume, int start, int end)
{
    from_3D_volume(volume, start, end);
}

void VolumeStack::from_3D_volume(Volume2DX volume, int start, int end)
{
    _stack.clear();
    
    if(start == -1) start = 0;
    if(end == -1) end = volume.nz()-1;
    if(end >= volume.nz())
    {
        std::cout << "WARNING: The end index exceeds the total number of frames possible, setting it to maximum possible value.\n";
        end = volume.nz()-1;
    }
    
    set_number_of_frames(end-start+1);

    _header = volume.header();
    
    std::cout << "Separating stacks.. ";
    clock_t start_time = clock();
    RealSpaceData data = volume.get_real();
    for(int frame_no=start; frame_no <= end; ++frame_no)
    {
        RealSpaceData data_frame(volume.nx(), volume.ny(), 1);
        for(int ix=0; ix < volume.nx(); ++ix)
        {
            for(int iy=0; iy < volume.ny(); ++iy)
            {
                data_frame.set_value_at(ix, iy, 0, data.get_value_at(ix, iy, frame_no));
            }
        }
        set_frame(frame_no, data_frame);
    }
    std::cout << (clock()-start_time)/(double)CLOCKS_PER_SEC << " sec\n";
    
}

Volume2DX VolumeStack::to_3D_volume()
{   
    _header.set_mz(number_of_frames());
    _header.set_sections(number_of_frames());
    Volume2DX volume3D(_header);
    
    std::cout << "Combining stacks.. ";
    clock_t start = clock();
    RealSpaceData data(_header.rows(), _header.columns(), number_of_frames());
    for(int frame_no=0; frame_no < number_of_frames(); ++frame_no)
    {
        RealSpaceData frame_data = get_frame(frame_no);
        
        for(int ix=0; ix < _header.rows(); ++ix)
        {
            for(int iy=0; iy < _header.columns(); ++iy)
            {
                double value = frame_data.get_value_at(ix, iy, 0);
                data.set_value_at(ix, iy, frame_no, value);
                
            }
        }
    }
    std::cout << (clock()-start)/(double)CLOCKS_PER_SEC << " sec\n";
    
    
    volume3D.set_real(data);
    return volume3D;
}

size_t VolumeStack::number_of_frames()
{
    return _stack.size();
}

void VolumeStack::set_number_of_frames(size_t frames)
{
    _stack.resize(frames);
}

RealSpaceData VolumeStack::get_frame(int frame_number)
{
    RealSpaceData data;
    if(frame_number >= number_of_frames())
    {
        std::cout << "WARNING: The frame number exceeds the total number.\n";
    }
    else
    {
        data = _stack[frame_number];
    }
    
    return data;
}

void VolumeStack::set_frame(int frame_number, const RealSpaceData& data)
{
    if(frame_number >= number_of_frames())
    {
        std::cout << "WARNING: The frame number exceeds the total number.\n";
    }
    else
    {
        _stack[frame_number] = data;
    }
    
}

VolumeHeader VolumeStack::header()
{
    return _header;
}


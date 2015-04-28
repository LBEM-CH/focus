/* 
 * @license GNU Public License
 * @author Nikhil Biyani (nikhilbiyani@gmail.com)
 * 
 */


#include <vector>

#include "density_generator.hpp"

volume::data::RealSpaceData volume::utilities::density_generator::create_density(int box_size, double max_resolution, double charge)
{
    std::cout << "Generating density with charge: " << charge << " electrons in a square box of size: "
              << box_size << " X " << box_size << " X " << box_size << "\n";

    volume::data::RealSpaceData data(box_size, box_size, box_size);

    double gauss_real_width = max_resolution / (M_PI); // in Angstrom, res is in Angstrom

    double min_table_val = 1e-7;
    double max_table_x = sqrt(-log(min_table_val)); // for exp(-x*x)

    double table_step_size = 0.001; // number of steps for each pixel
    double inv_table_step_size = 1.0 / table_step_size;
    int table_size = int (max_table_x * gauss_real_width / (table_step_size) * 1.25);
    std::vector<double> table;
    table.resize(table_size);

    for ( int i = 0; i < table_size; i++ ) {
        double x = -i * table_step_size / gauss_real_width;
        table[i] = exp(-x * x);
    }

    int gbox = int (max_table_x * gauss_real_width); // local box half size in pixels to consider for each point
    if ( gbox <= 0 )
        gbox = 1;

    double xc = box_size / 2;
    double yc = box_size / 2;
    double zc = box_size / 2;
    double fval = charge;
    int imin = int (xc) - gbox, imax = int (xc) + gbox;
    int jmin = int (yc) - gbox, jmax = int (yc) + gbox;
    int kmin = int (zc) - gbox, kmax = int (zc) + gbox;
    if ( imin < 0 )
        imin = 0;
    if ( jmin < 0 )
        jmin = 0;
    if ( kmin < 0 )
        kmin = 0;
    if ( imax > box_size )
        imax = box_size;
    if ( jmax > box_size )
        jmax = box_size;
    if ( kmax > box_size )
        kmax = box_size;

    for ( int k = kmin; k < kmax; k++ ) {
        size_t table_index_z = size_t(fabs(k - zc) * inv_table_step_size);
        if ( table_index_z >= table.size() ) continue;
        double zval = table[table_index_z];
        size_t pd_index_z = k * box_size * box_size;
        for ( int j = jmin; j < jmax; j++ ) {
            size_t table_index_y = size_t(fabs(j - yc) * inv_table_step_size);
            if ( table_index_y >= table.size() ) continue;
            double yval = table[table_index_y];
            size_t pd_index = pd_index_z + j * box_size + imin;
            for ( int i = imin; i < imax; i++, pd_index++ ) {
                size_t table_index_x = size_t(fabs(i - xc) * inv_table_step_size);
                if ( table_index_x >= table.size() ) continue;
                double xval = table[table_index_x];
                double current_value = data.get_value_at(pd_index);
                data.set_value_at(pd_index, current_value + (fval * zval * yval * xval));
            }
        }
    }

    std::cout << "Done\n";
    return data;
}

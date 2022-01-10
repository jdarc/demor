#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericMatrix mandelbrot_native(const double x_min, const double x_max, const double y_min, const double y_max,
                                      const int res_x, const int res_y, const int nb_iter) {
    Rcpp::NumericMatrix ret(res_x, res_y);
    double x_step = (x_max - x_min) / res_x;
    double y_step = (y_max - y_min) / res_y;
    int r, c;
    for (r = 0; r < res_y; r++) {
        for (c = 0; c < res_x; c++) {
            double zx = 0.0, zy = 0.0, new_zx;
            double cx = x_min + c * x_step, cy = y_min + r * y_step;
            int n = 0;
            for (n = 0; (zx * zx + zy * zy < 4.0) && (n < nb_iter); n++) {
                new_zx = zx * zx - zy * zy + cx;
                zy = 2.0 * zx * zy + cy;
                zx = new_zx;
            }
            ret(c, r) = n;
        }
    }
    return ret;
}

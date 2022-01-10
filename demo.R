library(Rcpp)
sourceCpp("mandelbrot.cpp", rebuild = T)

(function() {
    cols = colorRampPalette(c("blue", "yellow", "red", "black"))(11)

    naive <- function(x, y, c, z, k, nx, ny, n) {
        for (rep in 1:n) {
            for (i in 1:nx) {
                for (j in 1:ny) {
                    if (Mod(z[i, j]) < 2) {
                        z[i, j] <- z[i, j] ^ 2 + c[i, j]
                        k[i, j] <- k[i, j] + 1
                    }
                }
            }
        }
        return(list(x = x, y = y, k = k))
    }

    vectorized <- function(x, y, c, z, k, nx, ny, n) {
        for (rep in 1:n) {
            index <- which(Mod(z) < 2)
            z[index] <- z[index] ^ 2 + c[index]
            k[index] <- k[index] + 1
        }
        return(list(x = x, y = y, k = k))
    }

    mandelbrot <- function(xmin = -2, xmax = 2, ymin = -2, ymax = 2, nx = 500, ny = 500, n = 100, fn) {
        x <- seq(xmin, xmax, length.out = nx)
        y <- seq(ymin, ymax, length.out = ny)
        c <- outer(x, y * 1i, FUN = "+")
        z <- matrix(0.0, nrow = length(x), ncol = length(y))
        k <- matrix(0.0, nrow = length(x), ncol = length(y))

        return(fn(x, y, c, z, k, nx, ny, n))
    }

    print("Naive")
    print(system.time(n <- mandelbrot(fn = naive)))
    image(n$x, n$y, n$k, col = cols, xlab = "Re(c)", ylab = "Im(c)", useRaster = TRUE)

    print("Vectorised")
    print(system.time(v <- mandelbrot(fn = vectorized)))
    image(v$x, v$y, v$k, col = cols, xlab = "Re(c)", ylab = "Im(c)", useRaster = TRUE)

    print("Rcpp")
    print(system.time(m <- mandelbrot_native(-2, 2, -2, 2, 500, 500, 100)))
    image(m, col = cols, xlab = "Re(c)", ylab = "Im(c)", useRaster = TRUE)
})()

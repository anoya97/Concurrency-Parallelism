/*The Mandelbrot set is a fractal that is defined as the set of points c
in the complex plane for which the sequence z_{n+1} = z_n^2 + c
with z_0 = 0 does not tend to infinity.*/

/*This code computes an image of the Mandelbrot set.*/

#include <stdio.h>
#include <sys/time.h>

#define DEBUG 1

#define         X_RESN  1024     /* x resolution */
#define         Y_RESN  1024       /* y resolution */
#define         X_MIN   -2.0	/*Boundaries of the mandelbrot set*/
#define         X_MAX    2.0
#define         Y_MIN   -2.0
#define         Y_MAX    2.0
#define		maxIterations	1000 /*Cuanto mayor, más detalle en la imagen y mayor coste computacional*/
			


typedef struct complextype
        {
        float real, imag;
        } Compl;


int main ( )
{

       /* Mandelbrot variables */
        int i, j, k;
        Compl   z, c;
        float   lengthsq, temp;
        int res[X_RESN][Y_RESN]; 
		struct timeval  ti, tf;
		int microseconds;


		/* Start measuring time */
		gettimeofday(&ti, NULL);
         
        /* Calculate and draw points */
        for(i=0; i < X_RESN; i++) 
        for(j=0; j < Y_RESN; j++) {
          z.real = z.imag = 0.0;
          c.real = X_MIN + j * (X_MAX - X_MIN)/X_RESN;
          c.imag = Y_MAX - i * (Y_MAX - Y_MIN)/Y_RESN;
          k = 0;

          do  {    /* iterate for pixel color */

            temp = z.real*z.real - z.imag*z.imag + c.real;
            z.imag = 2.0*z.real*z.imag + c.imag;
            z.real = temp;
            lengthsq = z.real*z.real+z.imag*z.imag;
            k++;

          } while (lengthsq < 4.0 && k < maxIterations);

        if (k >= maxIterations) res[i][j] = 0;
        else res[i][j] = k;

        }
	
		/* End measuring time */
		gettimeofday(&tf, NULL);
		microseconds = (tf.tv_usec - ti.tv_usec)+ 1000000 * (tf.tv_sec - ti.tv_sec);
		/*printf ("(PERF) Time (seconds) = %lf\n", (double) microseconds/1E6);*/


	
	if( DEBUG ) {
		for(i=0;i<X_RESN;i++) {
			for(j=0;j<Y_RESN-1;j++)
		        	printf("%d\t", res[i][j]);
			printf( "%d\n", res[i][Y_RESN-1] );
		}
	}
}

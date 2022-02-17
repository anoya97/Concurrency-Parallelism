/*The Mandelbrot set is a fractal that is defined as the set of points c
in the complex plane for which the sequence z_{n+1} = z_n^2 + c
with z_0 = 0 does not tend to infinity.*/

/*This code computes an image of the Mandelbrot set.*/

#include <stdio.h>
#include <sys/time.h>
#include <mpi.h>
#include <stdlib.h>

#define DEBUG 0

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


int main (int argc, char * argv[] )
{

       /* Mandelbrot variables */
        int i, j, k;
        Compl   z, c;
        float   lengthsq, temp;
        int *res, *buffer;
		struct timeval  ti, tf;
		int microseconds;
		int operaciones;
		int numprocs, rank;

		MPI_Init(&argc,&argv);
		MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
		MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    res = malloc((X_RESN*Y_RESN)*sizeof(int));
    buffer = malloc((X_RESN*Y_RESN)*sizeof(int));




		/* Start measuring time */
		gettimeofday(&ti, NULL);
        /* Calculate and draw points */
        for(i=rank*(X_RESN * Y_RESN)/numprocs; i < (((rank+1)*(X_RESN * Y_RESN)/numprocs)); i+=Y_RESN)
        for(j=0; j < Y_RESN; j++) {
          z.real = z.imag = 0.0;
          c.real = X_MIN + j * (X_MAX - X_MIN)/X_RESN;
          c.imag = Y_MAX - ((i/X_RESN)) * (Y_MAX - Y_MIN)/Y_RESN;

          k = 0;

          do  {    /* iterate for pixel color */
            //printf("%d\n", k);
            temp = z.real*z.real - z.imag*z.imag + c.real;
            z.imag = 2.0*z.real*z.imag + c.imag;
            z.real = temp;
            lengthsq = z.real*z.real+z.imag*z.imag;
            k++;
            //printf("%f\n", lengthsq);Y_RESN)/Y_RESN)-1)*Y_RESNY_RESN)/Y_RESN)-1)*Y_RESN

          } while (lengthsq < 4.0 && k < maxIterations);

          if (k >= maxIterations) buffer[(((i+Y_RESN)/Y_RESN)-1)*Y_RESN+j] = 0;
          else buffer[(((i+Y_RESN)/Y_RESN)-1)*Y_RESN+j] = k;


          //if (rank==1) printf("%d\n",k);
        }

		/* End measuring time */
		gettimeofday(&tf, NULL);
		microseconds = (tf.tv_usec - ti.tv_usec)+ 1000000 * (tf.tv_sec - ti.tv_sec);
		printf ("(PERF) Time (seconds) = %lf\n", (double) microseconds/1E6);

		MPI_Gather(buffer+(Y_RESN*X_RESN*rank/numprocs),(X_RESN*Y_RESN)/numprocs,MPI_INT,res,(X_RESN*Y_RESN)/numprocs,MPI_INT,0,MPI_COMM_WORLD);

	if ( rank==0 )
	if( DEBUG ) {
    for(i=0; i < (X_RESN * Y_RESN) ; i+=Y_RESN){
			for(j=0;j<Y_RESN-1;j++)
		        	printf("%d\t", res[i+j]);
      printf("\n");
    }
	}
  MPI_Finalize();
}

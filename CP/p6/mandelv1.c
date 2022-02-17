/*The Mandelbrot set is a fractal that is defined as the set of points c
in the complex plane for which the sequence z_{n+1} = z_n^2 + c
with z_0 = 0 does not tend to infinity.*/

/*This code computes an image of the Mandelbrot set.*/

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <mpi.h>


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
        int * res;
        int * buffer;
		struct timeval  ti, tf;
		int microseconds;
		int numprocs, rank;
    int divided,part;
		/* Start measuring time */
		gettimeofday(&ti, NULL);

    MPI_Init(&argc,&argv);
		MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
		MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    buffer = (int *) malloc (sizeof(int)*X_RESN*Y_RESN/numprocs);

    if (rank == 0)
      res = (int *) malloc (sizeof(int)*X_RESN*Y_RESN);

    divided = X_RESN/numprocs;
    //printf("%d\n", divided);
        /* Calculate and draw points */
        for(i=0; i < divided; i++)
        for(j=0; j < Y_RESN; j++) {
          z.real = z.imag = 0.0;
          c.real = X_MIN + j * (X_MAX - X_MIN)/X_RESN;
          c.imag = Y_MAX - ((i+ (rank*Y_RESN)/numprocs)) * (Y_MAX - Y_MIN)/Y_RESN;
          k = 0;

          do  {    /* iterate for pixel color */

            temp = z.real*z.real - z.imag*z.imag + c.real;
            z.imag = 2.0*z.real*z.imag + c.imag;
            z.real = temp;
            lengthsq = z.real*z.real+z.imag*z.imag;
            k++;

          } while (lengthsq < 4.0 && k < maxIterations);

        if (k >= maxIterations) buffer[i*Y_RESN+j] = 0;
        else buffer[i*Y_RESN+j] = k;
        //if (rank == 1)
        //printf("%d %d\n", (i+ (rank*Y_RESN)/numprocs)+j, k);
        }


		/* End measuring time */
		gettimeofday(&tf, NULL);
		microseconds = (tf.tv_usec - ti.tv_usec)+ 1000000 * (tf.tv_sec - ti.tv_sec);
		printf ("proceso %d ,(PERF) Time (seconds) = %lf\n", rank, (double) microseconds/1E6);

    MPI_Gather(buffer, (X_RESN/numprocs)*Y_RESN, MPI_INT, res,(X_RESN/numprocs)*Y_RESN, MPI_INT,0,MPI_COMM_WORLD);


  if (rank==0){
  	if( DEBUG ) {
  		for(i=0;i<X_RESN;i++) {
  			for(j=0;j<Y_RESN;j++){
  		        	printf("%d\t", res[i*Y_RESN+j]);

                //part++;
        }
        printf( "\n");
  		}
      //printf("\n ala es grande %d\n", part);
  	}
  }
  MPI_Finalize();
}

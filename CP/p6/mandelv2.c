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
		struct timeval  ti, tf;
		int microseconds;
		int numprocs, rank;
    int fila;
    int fin = -1;
    MPI_Status status;
    MPI_Comm comm;
    int res[X_RESN][Y_RESN];

		/* Start measuring time */
		gettimeofday(&ti, NULL);

    MPI_Init(&argc,&argv);
		MPI_Comm_size(MPI_COMM_WORLD,&numprocs);
		MPI_Comm_rank(MPI_COMM_WORLD,&rank);

    int buf[Y_RESN];

    // root code
    if (rank == 0) {

      int filaenv = 0; //Filas Enviadas
      int filarec = 0; //Filas Recibidas

      for (i = 1; i < numprocs; i++) { //Envio de Filas iniciales
          MPI_Send( &filaenv, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
          filaenv++;
      }

      while (filarec < X_RESN) {

        /* Recibimos la fila calculada*/
        MPI_Recv(&buf, Y_RESN, MPI_INT, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        filarec++;

        if (filaenv < X_RESN){ // Si quedan filas por enviar
          MPI_Send(&filaenv, 1, MPI_INT, status.MPI_SOURCE,0,MPI_COMM_WORLD);
          filaenv++;
        } else { // Envio de mensaje de fin de trabajo
          MPI_Send(&fin, 1, MPI_INT, status.MPI_SOURCE,0,MPI_COMM_WORLD);
        }

        /* Pasamos la fila calculada al la matriz final*/
        for (i = 0; i < Y_RESN; i++) {
          res[status.MPI_TAG][i] = buf[i];
        }

      }

    } else { // Workers code

      while (1) {

        MPI_Recv(&fila,1,MPI_INT,0,0,MPI_COMM_WORLD,&status);

        if (fila == fin) //El mensaje recibido es el de fin de trabajo
          break;

        /* Calculate and draw points */
        for(j=0; j < Y_RESN; j++) {
          z.real = z.imag = 0.0;
          c.real = X_MIN + j * (X_MAX - X_MIN)/X_RESN;
          c.imag = Y_MAX - fila * (Y_MAX - Y_MIN)/Y_RESN;
          k = 0;

          do  {    /* iterate for pixel color */

            temp = z.real*z.real - z.imag*z.imag + c.real;
            z.imag = 2.0*z.real*z.imag + c.imag;
            z.real = temp;
            lengthsq = z.real*z.real+z.imag*z.imag;
            k++;

          } while (lengthsq < 4.0 && k < maxIterations);

        if (k >= maxIterations) buf[j] = 0;
        else buf[j] = k;
        }

        MPI_Send(&buf, X_RESN, MPI_INT, 0, fila, MPI_COMM_WORLD);

      }
    }

		/* End measuring time
		gettimeofday(&tf, NULL);
		microseconds = (tf.tv_usec - ti.tv_usec)+ 1000000 * (tf.tv_sec - ti.tv_sec);
		printf ("proceso %d ,(PERF) Time (seconds) = %lf\n", rank, (double) microseconds/1E6);
    */

    if (rank == 0)
    if( DEBUG ) {
  		for(i=0;i<X_RESN;i++) {
  			for(j=0;j<Y_RESN;j++)
  		    printf("%d\t", res[i][j]);
  			printf( "\n");
  		}
  	}
  MPI_Finalize();
}

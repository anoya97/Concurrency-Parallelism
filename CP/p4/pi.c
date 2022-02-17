#include <stdio.h>
#include <math.h>
#include <mpi.h>

int main(int argc, char *argv[])
{
    int i, done = 0, n,aux;
    double PI25DT = 3.141592653589793238462643;
    double pi, h, sum, x, daux;
    int numprocs, rank;
    MPI_Status status;

    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    while (!done)
    {
      if (rank == 0) {

          printf("Enter the number of intervals: (0 quits) \n");
          scanf("%d",&n);
          aux = n;

          for (i = 1; i < numprocs; i++) {
              MPI_Send(&aux, 1, MPI_INT, i, 0, MPI_COMM_WORLD);
          }

      }else{

        MPI_Recv(&n, 1, MPI_INT,0, 0, MPI_COMM_WORLD, &status);
      }

        if (n == 0) break;

        h = 1.0 / (double) n;
        sum = 0.0;
        
        for (i = rank+1; i <= n; i+=numprocs) {
            x = h * ((double)i - 0.5);
            sum += 4.0 / (1.0 + x*x);
        }

        if (rank == 0) {

          for (i = 1; i < numprocs; i++) {
              MPI_Recv(&daux, 1, MPI_DOUBLE,
                i, 0, MPI_COMM_WORLD, &status);
              sum+= daux;
          }

          pi = h * sum;
          printf("pi is approximately %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));

        } else {
          MPI_Send(&sum, 1, MPI_DOUBLE,0, 0, MPI_COMM_WORLD);
        }

    }
    MPI_Finalize();
}

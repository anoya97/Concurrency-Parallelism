#include <stdio.h>
#include <math.h>
#include <mpi.h>




int MPI_FattreeColectiva(void *buf, int count, MPI_Datatype datatype,int root, MPI_Comm comm)
{
  int numprocs, rank,i;
  MPI_Status status;


  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);

  if (rank == root) {
    for (i = 1; i < numprocs; i++) {
        MPI_Send(buf, count, datatype,
          i, root, comm);
    }
  }else{
    MPI_Recv(buf, count, datatype,
      root, 0, comm, &status);
  }
}

int MPI_BinomialColectiva(void *buf, int count, MPI_Datatype datatype,
                            int root, MPI_Comm comm)
{
  int numprocs, rank,a;
  MPI_Status status;

  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &rank);


  for (int i = 1; i < log2(numprocs); i++) {
    printf("proceso %d CICLO %d\n",rank,i);
    fflush(0);
    if (rank < pow(2,(i-1))){
      printf("proceso %d CICLO %d\n",rank,i);
      fflush(0);
      if(numprocs > ((int)(rank+pow(2,(i-1))))){  //Impide enviar en cuando el numero de procesos no es una potencia de 2
          printf("Envio mensaje, Proceso: %d\n", rank);
          fflush(0);
          MPI_Send(buf, count, datatype, (rank+ ((int)pow(2,(i-1)))), 0, comm);  //
        }
    } else{
      if(rank < pow(2,i)){
        printf("ciclo %d, %d de %d \n",i, rank,(rank-((int) pow(2,(i-1)))));
        fflush(0);
        MPI_Recv(buf, count, datatype, (rank-((int) pow(2,(i-1)))),0, comm, &status);
      }
    }
  }

}

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
      }

      //MPI_Bcast(&n, 1, MPI_INT,0, MPI_COMM_WORLD);
      MPI_BinomialColectiva(&n, 1, MPI_INT,0, MPI_COMM_WORLD);

      //printf("%d\n", n);
      if (n == 0) break;

      h   = 1.0 / (double) n;
      sum = 0.0;
      for (i = rank+1; i <= n; i+=numprocs) {
        x = h * ((double)i - 0.5);
        sum += 4.0 / (1.0 + x*x);
      }

      MPI_Reduce(&sum,&pi,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);

      if (rank == 0) {
        pi = h * pi;
        printf("pi is approximately %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));
      }

    }
    MPI_Finalize();
}

/* Integrantes:
 * Raúl Fernández del Blanco (r.delblanco)
 * Armando Martínez Noya (a.mnoya)
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

int main(int argc, char *argv[])
{
    int i, done = 0, n;
    double PI25DT = 3.141592653589793238462643;
    double pi, h, total, x;

    int k;
    double aprox;
    int totalprocs, process;

    MPI_Status status;

    //Inicializamos el entorno MPI.
    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &totalprocs); //Toma el número de procesos.
    MPI_Comm_rank(MPI_COMM_WORLD, &process); //Toma su número de proceso.

    while (!done)
    {
        if(process==0){ //El proceso 0 es el que pide los datos.
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d",&n);

            for(k = 1; k < totalprocs; k++){ //Enviamos el valor de n a todos los procesos.
                MPI_Send(&n, 1, MPI_INT, k, 0, MPI_COMM_WORLD);
            }
        }else{ //El resto de procesos reciben el valor de n.
            MPI_Recv(&n, 1, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
        }

        if (n == 0) break; //Termina el programa

        //Calculo de aproximaciones parciales de PI (Formulas de internet).
        h   = 1.0 / (double) n;
        total = 0.0;
        for (i = process + 1; i <= n; i += totalprocs) {
            x = h * ((double)i - 0.5);
            total += 4.0 / (1.0 + x*x);
        }
        pi = h * total;

        //En este punto todos los procesos tienen su valor local de PI

        if (process > 0){ //El resto de procesos envian las aproximaciones parciales al proceso 0.

            MPI_Send(&pi, 1, MPI_DOUBLE, 0, 0, MPI_COMM_WORLD);

        } else{//El proceso 0 se encarga de recolectar las aprox parciales e imprimir.

            for (k = 1; k < totalprocs; k++)
            {
                MPI_Recv(&aprox, 1, MPI_DOUBLE, MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
                pi += aprox;
            }
            printf("pi is approximately %.16f, Error is %.16f\n", pi, fabs(pi - PI25DT));

        }
    }
    //Esperamos por todos los procesos y liberamos todos los recursos reservados.
    MPI_Finalize();
}
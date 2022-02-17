
MPI_Colectiva

for (int i = 0; i < nº etapas; ++i) {
  if (me toca recibir) {
      MPI_Recv  //MPI_ANY_SOURCE -> no usar
  } else if (me toca enviar){
    MPI_Send
  }

}


gdb --attach pid 

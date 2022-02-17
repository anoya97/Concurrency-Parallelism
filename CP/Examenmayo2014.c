

/* Ejercicio 1 */

#define N . . . // Numero de threads en el anillo
#define PREV( i ) ( i == 0) ? (N−1): ( i−1))
#define NEXT( i ) (( i+1)% N)

queue ∗q[N] ; // Cola de cada thread
pthread mutex t lock [N] ; // mutex para proteger la cola de cada thread
pthread cond t empty[N] ;
element remove(queue ∗q) ; // Quitar elemento de cola
void insert(element e , queue ∗q) ; // Insertar elemento en cola
int size(queue ∗q) ; // Devuelve el numero de elementos en la cola

int nodo(int num) // Num es el numero de thread dentro del anillo
{
  element ∗old , ∗new;
  int next =NEXT(num) ; // Siguiente en el anillo
  int prev =PREV(num) ; // Anterior en el anillo
  queue * insert_q;
  int insert_thread;

  while(1) {
    pthread_mutex_lock(&lock [num]) ;
    if ((size(q[num])==0) && (size(q2[num])==0))
      pthread_cond_wait(&empty[num] , &lock [num]) ;

    if (size(q[num])==0){
      old = remove(q[num]);
      insert_q = q[next];
      insert_thread = next;
    }else{
      old = remove(q2[num]);
      insert_q = q[prev];
      insert_thread = prev;
    }

    pthread_mutex_unlock(&lock [num] ) ;

    new = process(old);

    pthread_mutex_lock(&lock[insert_thread] ) ;
    insert(new, insert_q);
    if (size(insert_q)==1)
      pthread_cond_signal(&empty[insert_thread]) ;

    pthread mutex unlock(&lock[insert_thread]) ;
  }
}
int pthread mutex lock(pthread mutex t ∗mutex) ;
int pthread mutex trylock(pthread mutex t ∗mutex) ;
int pthread mutex unlock(pthread mutex t ∗mutex) ;
int pthread cond wait(pthread cond t ∗cond, pthread mutex t ∗mutex) ;
int pthread cond broadcast(pthread cond t ∗cond) ;
int pthread cond signal(pthread cond t ∗cond) ;

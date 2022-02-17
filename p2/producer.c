#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include "options.h"
#include "list.h"

#define LIST_SIZE 1000

/* creation and consumption of elements */

/**
 * Element: Describe one of the elements that we work with
 *
 * @producer: which thread has created the element
 * @value: What is the producer value
 * @time: how long it took to generate the value in microseconds
 */
struct element {
	int producer;
	int value;
	int time;
};

/**
 * thread_info: Information needed for each created thread
 *
 * @thread_id: pthread identifier for the thread
 * @thread_num: numerical identifier for our application
 */
struct thread_info {    /* Used as argument to thread_start() */
	pthread_t thread_id;        /* ID returned by pthread_create() */
	int       thread_num;       /* Application-defined thread # */
};

/**
 * producer_args: Arguments passed to each producer thread
 *
 * @thread_num: numerical identifier for our application
 * @queue: queue into which insert produced elements
 */
struct producer_args {
	int thread_num;
	struct queue *queue;
    pthread_mutex_t *create_mutex;
    int *created_elements;

    int *cont_buf_lleno; //cuenta el numero de veces que el buffer se encuentra lleno
    pthread_mutex_t *mutex_cont_buf; //mutex que protege la variable con_buf_lleno
    int count_elem; //cuenta el numero de elementos que produce cada productor
    int *p_iterations;
    pthread_mutex_t *p_iter_mutex;
};

struct thread_buff_lleno{ // almacena los argumentos del thread que controla si el buffer esta lleno y crea un nuevo consumidor si
	list consumers;       // es necesario
	struct options opt;
	struct queue *queue;
	int *c_iterations;                
    pthread_mutex_t *c_iter_mutex;	  
	int *cont_buf_lleno;
	pthread_mutex_t *mutex_cont_buf;
	int *consumed_elements;
	pthread_mutex_t *consume_mutex;
	int *thread_num_con;

	int *cont_buf_vacio; 
    pthread_mutex_t *mutex_buf_vacio;
    int muerto;

};

struct thread_buff_vacio{ // almacena los argumentos del thread que controla si el buffer esta vacio y mata un consumidor si
	list consumers;       // es necesario
	int *c_iterations;                
    pthread_mutex_t *c_iter_mutex;	  

	int *cont_buf_vacio; 
    pthread_mutex_t *mutex_buf_vacio;
};

/**
 * consumer_args: Arguments passed to each consumer thread
 *
 * @thread_num: numerical identifier for our application
 * @queue: queue from where to remove elements
 */
struct consumer_args {
	int thread_num;
	struct queue *queue;
    pthread_mutex_t *consume_mutex;
    int *consumed_elements;
    
    int *cont_buf_vacio; //cuenta el numero de veces que el buffer se encuentra vacio
    pthread_mutex_t *mutex_buf_vacio; //mutex que protege la variable con_buf_vacio
    int muerto; //entero que controla cuando muere un consumidor
    int consumed_elements_simple; //contador de elementos consumidos por cada thread
    int *c_iterations;
    pthread_mutex_t *c_iter_mutex;
};


/**
 * element_create: creates a new element
 *
 * @producer: thread that produces it
 * @seed: this is needed for the random generator to work
 */
struct element *element_create(struct producer_args *arg, unsigned int *seed)
{
	struct element *e = malloc(sizeof(*e));
	if (e == NULL) {
		printf("Ont of memory");
		exit(-1);
	}
	e->producer = arg->thread_num;
	e->value = rand_r(seed) % 1000;
	e->time = rand_r(seed) % 100000;
	usleep(e->time);

	pthread_mutex_lock(arg->create_mutex);
	(*arg->created_elements)++;
	pthread_mutex_unlock(arg->create_mutex);
	return e;
}



/**
 * element_consume: creates a new element
 *
 * @producer: thread that produces it
 * @seed: this is needed for the random generator to work
 */
void element_consume(struct consumer_args *arg, struct element *e)
{
	usleep(e->time);
	free(e);
	arg->consumed_elements_simple++;
	pthread_mutex_lock(arg->consume_mutex);
	(*arg->consumed_elements)++;
	pthread_mutex_unlock(arg->consume_mutex);
}

/* queue implementation */

/**
 * Queue: Queue of elements
 *
 * @elements: array of elements that would store the values
 * @count: number of used elements
 * @size: size in elements of elements array
 * @insertions: how many insertions have we got in the whole lifetime
 */
struct queue {
	struct element **elements;
	int count;
	int size;
	int insertions;
    int removals;
    
    pthread_cond_t buffer_full;
    pthread_cond_t buffer_empty;

    pthread_mutex_t buffer_mutex;
};

/**
 * queue_create: create a new queue
 *
 * @size: size of the queue
 */
static struct queue *queue_create(int size, int iterations)
{
	struct queue *queue =  malloc(sizeof(struct queue));
	queue->size = size;
	queue->count = 0;
	queue->insertions = 0;
    queue->removals = 0;
	queue->elements = malloc(sizeof(struct element) * size);

    pthread_cond_init(&queue->buffer_empty, NULL);
    pthread_cond_init(&queue->buffer_full, NULL);
    
    pthread_mutex_init(&queue->buffer_mutex, NULL);
	return queue;
}

/**
 * queue_destroy: destroy a queue
 *
 * Destroy the queue and frees its memory
 *
 * @queue: queue to destroy
 */
static void queue_destroy(struct queue *queue)
{
	if (queue->count != 0) {
		printf("queue still has %d elements\n", queue->count);
	}
	free(queue->elements);
	free(queue);
}

/**
 * queue_is_empty: returns true if the queue is empty
 *
 * @queue: queue to check
 */
static bool queue_is_empty(struct queue *queue)
{
	return queue->count == 0;
}

/**
 * queue_is_full: returns true if the queue is full
 *
 * @queue: queue to check
 */
static bool queue_is_full(struct queue *queue)
{
	return queue->count == queue->size;
}

/**
 * queue_insertions: returns the number of insertions
 *
 * @queue: queue to check
 */
static int queue_insertions(struct queue *queue)
{
	return queue->insertions;
}

/**
 * queue_removals: returns the number of removes
 *
 * @queue: queue to check
 */
static int queue_removals(struct queue *queue)
{
	return queue->removals;
}


/**
 * queue_insert: inserts a new element on the queue
 *
 * @queue: queue where to insert
 * @element: element to insert
 */
static void queue_insert(struct queue *queue, struct element *element)
{
	if (queue->count == queue->size) {
		printf("buffer is full\n");
		exit(-1);
	}
	queue->elements[queue->count] = element;
	queue->count++;
	queue->insertions++;
}
/**
 * queue_remove: remove an element in the queue
 *
 * Returns the element removed
 *
 * @queue: queue where to remove
 */
struct element *queue_remove(struct queue *queue)
{
	if (queue->count == 0) {
		printf("buffer is empty\n");
		exit(-1);
	}
	queue->count--;
    queue->removals++;
	return queue->elements[queue->count];
}

/**
 * producer_function: function executed by each producer thread
 *
 * @ptr: producer_args that needs to be passed as void
 */
void *producer_function(void *ptr)
{
	struct producer_args *args = ptr;
	unsigned int seed = args->thread_num;

	printf("producer thread %d\n", args->thread_num);
	while(1) {
		pthread_mutex_lock(args->p_iter_mutex);
		if(*(args->p_iterations) == 0){
			pthread_mutex_unlock(args->p_iter_mutex);
			break;
		}
		(*(args->p_iterations))--;
		pthread_mutex_unlock(args->p_iter_mutex);

		bool is_empty;
		struct element *e = element_create(args, &seed);

		printf("%d: produces %d in %d microseconds\n", args->thread_num, e->value, e->time);
		pthread_mutex_lock(&args->queue->buffer_mutex);

		if(queue_is_full(args->queue)){
			pthread_mutex_lock(args->mutex_cont_buf);
			(*(args->cont_buf_lleno))++;
			pthread_mutex_unlock(args->mutex_cont_buf);	
			//printf("BUFFER LLENO\n");
		}
		while(queue_is_full(args->queue)){
			pthread_cond_wait(&args->queue->buffer_full, &args->queue->buffer_mutex);
		}
		is_empty = queue_is_empty(args->queue);
		args->count_elem++;
		queue_insert(args->queue, e);
		if(is_empty)
			pthread_cond_broadcast(&args->queue->buffer_empty);
		pthread_mutex_unlock(&args->queue->buffer_mutex);
	}
	return NULL;
}


/**
 * consumer_function: function executed by each consumer thread
 *
 * @ptr: consumer_args that needs to be passed as void
 */
void *consumer_function(void *ptr)
{
	struct consumer_args *args = ptr;

	printf("consumer thread %d\n", args->thread_num);
	while(1) {

		if (args->muerto == 1){
			return NULL;
		}

		pthread_mutex_lock(args->c_iter_mutex);
		if(*(args->c_iterations) == 0){
			pthread_mutex_unlock(args->c_iter_mutex);
			break;
		}
		(*(args->c_iterations))--;
		pthread_mutex_unlock(args->c_iter_mutex);

		struct element *e;
		bool is_full;
		pthread_mutex_lock(&args->queue->buffer_mutex);

		if(queue_is_empty(args->queue)){
			pthread_mutex_lock(args->mutex_buf_vacio);
			(*(args->cont_buf_vacio))++;
			pthread_mutex_unlock(args->mutex_buf_vacio);	
			//printf("BUFFER VACIO\n");
		}

		while(queue_is_empty(args->queue))
			pthread_cond_wait(&args->queue->buffer_empty, &args->queue->buffer_mutex);
		is_full = queue_is_full(args->queue);
		e = queue_remove(args->queue);
		if(is_full)
			pthread_cond_broadcast(&args->queue->buffer_full);
		pthread_mutex_unlock(&args->queue->buffer_mutex);

		printf("%d: consumes %d in %d microseconds\n", args->thread_num, e->value, e->time);
		element_consume(args, e);
	}
	return NULL;
}


struct producer {
    struct thread_info   info;
    struct producer_args args;
};

struct consumer {
    struct thread_info   info;
    struct consumer_args args;
};

/**
 * producer_create: creates a new producer
 */
void producer_create(list l, int thread_num, struct queue *queue, int *created_elements, int *p_iterations,pthread_mutex_t *create_mutex,
	pthread_mutex_t *p_iter_mutex,struct options opt,int *cont_buf_lleno, pthread_mutex_t *mutex_cont_buf)
{
    struct producer *p=malloc(sizeof(struct producer));

    p->args.cont_buf_lleno = cont_buf_lleno;
    p->args.mutex_cont_buf = mutex_cont_buf;
	p->info.thread_num = thread_num;
	p->args.thread_num = thread_num;
	p->args.queue = queue;
    p->args.created_elements = created_elements;
    p->args.create_mutex = create_mutex;

    p->args.p_iterations = p_iterations;
    p->args.p_iter_mutex = p_iter_mutex;
    
    insert_element(l, p);

	if ( 0 != pthread_create(&p->info.thread_id, NULL,
				 producer_function, &p->args)) {
		printf("Failing creating producer thread %d", thread_num);
		exit(1);
	}
}

/**
 * consumer_create: creates a new consumer
 *
 */
void consumer_create(list l, int thread_num, struct queue *queue, int *consumed_elements,int *c_iterations, pthread_mutex_t *consume_mutex,
	pthread_mutex_t *c_iter_mutex, struct options opt, int *cont_buf_vacio, pthread_mutex_t *mutex_buf_vacio ,int muerto)
{
    struct consumer *c=malloc(sizeof(struct consumer));

    c->args.cont_buf_vacio = cont_buf_vacio;
    c->args.mutex_buf_vacio = mutex_buf_vacio;
	c->info.thread_num = thread_num;
	c->args.thread_num = thread_num;
	c->args.queue = queue;
    c->args.consumed_elements = consumed_elements;
    c->args.consume_mutex = consume_mutex;
    c->args.muerto = muerto;

    c->args.c_iterations = c_iterations;
    c->args.c_iter_mutex = c_iter_mutex;

    insert_element(l, c);

	if ( 0 != pthread_create(&c->info.thread_id, NULL,
				 consumer_function, &c->args)) {
		printf("Failing creating consumer thread %d", thread_num);
		exit(1);
	}
}


void *buffer_is_full_function(void *ptr){
	struct thread_buff_lleno *args = ptr;

	while(1){
		pthread_mutex_lock(args->mutex_cont_buf);
		*(args->cont_buf_lleno) = 0;
		pthread_mutex_unlock(args->mutex_cont_buf);

		sleep(1);

		pthread_mutex_lock(args->c_iter_mutex);
		if(*(args->c_iterations) == 0){
			pthread_mutex_unlock(args->c_iter_mutex);
			break;
		}
		pthread_mutex_unlock(args->c_iter_mutex);

		pthread_mutex_lock(args->mutex_cont_buf);
		if(*(args->cont_buf_lleno) >= 10){
			pthread_mutex_unlock(args->mutex_cont_buf);

			printf("CREANDO CONSUMIDOR+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");

			consumer_create(args->consumers,*args->thread_num_con,args->queue,args->consumed_elements,args->c_iterations,args->consume_mutex
				,args->c_iter_mutex, args->opt, args->cont_buf_vacio, args->mutex_buf_vacio ,args->muerto);

			(*(args->thread_num_con))++;
		}
		pthread_mutex_unlock(args->mutex_cont_buf);
	}
	return NULL;
}

void *buffer_is_vacio_function(void *ptr){
	struct thread_buff_vacio *args = ptr;
	iterator it;
	while(1){
		pthread_mutex_lock(args->mutex_buf_vacio);
		*(args->cont_buf_vacio) = 0;
		pthread_mutex_unlock(args->mutex_buf_vacio);

		sleep(1);

		pthread_mutex_lock(args->c_iter_mutex);
		if(*(args->c_iterations) == 0){
			pthread_mutex_unlock(args->c_iter_mutex);
			break;
		}
		pthread_mutex_unlock(args->c_iter_mutex);

		pthread_mutex_lock(args->mutex_buf_vacio);
		if (*(args->cont_buf_vacio) >= 10){
			pthread_mutex_unlock(args->mutex_buf_vacio);
			printf("MATANDO CONSUMIDOR-------------------------------------------------------------------------------------------\n");

			for (it = get_iterator(args->consumers); !is_end(it); next(it)) {
		        struct consumer *c= get_element(it);
		        if(c->args.muerto == 0){
		        	c->args.muerto = 1;
		        	break;
		        }
			}
		    free_iterator(it);

		}
		pthread_mutex_unlock(args->mutex_buf_vacio);
	}
	return NULL;
}

/**
 * producers_consumer: creates and wait to finish for producers and consumers
 *
 * @opt: command line options
 */
void producers_consumers(struct options opt)
{
	int i;
	int j;
	struct queue *queue;
    iterator it;

    struct thread_info thread_con;
    struct thread_buff_lleno thread_buff_lleno;

    struct thread_buff_vacio thread_buff_vacio;
    struct thread_info thread_con_vacio;

    int cont_buf_lleno = 0;
    int cont_buf_vacio = 0;
    int muerto = 0;

    list producers;
    list consumers;

    int created_elements=0;
    int consumed_elements=0;
    
    int p_iterations = opt.iterations;
    int c_iterations = opt.iterations;
    
    pthread_mutex_t create_mutex;
    pthread_mutex_t consume_mutex;

    pthread_mutex_t c_iter_mutex;
    pthread_mutex_t p_iter_mutex;
    pthread_mutex_t mutex_cont_buf;
    pthread_mutex_t mutex_buf_vacio;

    pthread_mutex_init(&create_mutex, NULL);
    pthread_mutex_init(&consume_mutex, NULL);

    pthread_mutex_init(&c_iter_mutex, NULL);
    pthread_mutex_init(&p_iter_mutex, NULL);
    pthread_mutex_init(&mutex_cont_buf, NULL);
    pthread_mutex_init(&mutex_buf_vacio, NULL);

	printf("creating buffer with %d elements\n", opt.buffer_size);
	queue = queue_create(opt.buffer_size,opt.iterations);

	if (queue == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

    producers=create_list(LIST_SIZE);
    consumers=create_list(LIST_SIZE);


	printf("creating %d producers\n", opt.num_producers);


	thread_buff_lleno.consumers = consumers;
	thread_buff_lleno.opt = opt;
	thread_buff_lleno.queue = queue;
	thread_buff_lleno.c_iterations = &c_iterations;
	thread_buff_lleno.c_iter_mutex = &c_iter_mutex;
	thread_buff_lleno.cont_buf_lleno = &cont_buf_lleno;
	thread_buff_lleno.mutex_cont_buf = &mutex_cont_buf;
	thread_buff_lleno.consumed_elements = &consumed_elements;
	thread_buff_lleno.consume_mutex = &consume_mutex;
	thread_buff_lleno.thread_num_con = &i;
	thread_buff_lleno.cont_buf_vacio = &cont_buf_vacio;
	thread_buff_lleno.mutex_buf_vacio = &mutex_buf_vacio;
	thread_buff_lleno.muerto = muerto;


	if ( 0 != pthread_create(&(thread_con.thread_id), NULL,   //Se crea el thread que controla si el buffer esta lleno
				 buffer_is_full_function, &thread_buff_lleno)) {
		printf("Failing creating thread_cont thread");
		exit(1);
	}


	thread_buff_vacio.consumers = consumers;
	thread_buff_vacio.c_iterations = &c_iterations;
	thread_buff_vacio.c_iter_mutex = &c_iter_mutex;
	thread_buff_vacio.cont_buf_vacio = &cont_buf_vacio;
	thread_buff_vacio.mutex_buf_vacio = &mutex_buf_vacio;
	

	if ( 0 != pthread_create(&(thread_con_vacio.thread_id), NULL,   //Se crea el thread que controla si el buffer esta vacio
				 buffer_is_vacio_function, &thread_buff_vacio)) {
		printf("Failing creating thread_cont thread");
		exit(1);
	}

	for (j = 0; j < opt.num_producers; j++) {
		producer_create(producers, j, queue, &created_elements, &p_iterations,&create_mutex,&c_iter_mutex,opt,&cont_buf_lleno,&mutex_cont_buf);
	}

	printf("creating %d consumers\n", opt.num_consumers);

	for (i = 0; i < opt.num_consumers; i++) {
		consumer_create(consumers, i, queue, &consumed_elements,&c_iterations, &consume_mutex,&p_iter_mutex,opt,&cont_buf_vacio,&mutex_buf_vacio,
			muerto);
	}

    
	for (it = get_iterator(producers); !is_end(it); next(it)) {
		struct producer *p= get_element(it);
        pthread_join(p->info.thread_id, NULL);
	}
    free_iterator(it);
    
	for (it = get_iterator(consumers); !is_end(it); next(it)) {
        struct consumer *c= get_element(it);
		pthread_join(c->info.thread_id, NULL);
	}
    free_iterator(it);

    pthread_join(thread_con.thread_id,NULL);

    pthread_join(thread_con_vacio.thread_id,NULL);
    
    printf("%d elements created. %d elements inserted into the queue\n", created_elements, queue_insertions(queue));
    printf("%d elements consumed. %d elements removed from the queue\n\n", consumed_elements, queue_removals(queue));

    //Se imprime la informacion de los elementos producidos por cada thread
    for (it = get_iterator(producers); !is_end(it); next(it)) {
		struct producer *p= get_element(it);
        printf("Producer %i produces: %i elements\n",p->info.thread_num,p->args.count_elem); 
	}
	free_iterator(it);

	printf("\n");

	//Se imprime el numero de elementos consumidos por cada thread
	for (it = get_iterator(consumers); !is_end(it); next(it)) {
		struct consumer *c= get_element(it);
		printf("Consumer %i consumes: %i elements\n",c->info.thread_num,c->args.consumed_elements_simple);
    }    
    free_iterator(it);

    printf("\n");

    for (it = get_iterator(consumers); !is_end(it); next(it)) {
		struct consumer *c= get_element(it);
		if(c->args.muerto == 1){
			printf("Consumers %i matado \n",c->info.thread_num);
		}
    }    
    free_iterator(it);

	free_list(producers, free);
	free_list(consumers, free);
	queue_destroy(queue);
}

int main (int argc, char **argv)
{
	struct options opt;

	opt.num_producers = 3;
	opt.num_consumers = 3;
	opt.buffer_size = 5;
	opt.iterations = 100;

	read_options(argc, argv, &opt);

	producers_consumers(opt);

	pthread_exit (0);
}

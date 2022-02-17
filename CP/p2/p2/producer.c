
#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdbool.h>
#include "options.h"

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

pthread_mutex_t create_mutex = PTHREAD_COND_INITIALIZER;
int created_elements = 0;;

/**
 * element_create: creates a new element
 *
 * @producer: thread that produces it
 * @seed: this is needed for the random generator to work
 */
struct element *element_create(int producer, unsigned int *seed)
{
	struct element *e = malloc(sizeof(*e));
	if (e == NULL) {
		printf("Ont of memory");
		exit(-1);
	}
	e->producer = producer;
	e->value = rand_r(seed) % 1000;
	e->time = rand_r(seed) % 100000;
	usleep(e->time);

	pthread_mutex_lock(&create_mutex);
	created_elements++;
	pthread_mutex_unlock(&create_mutex);
	return e;
}

pthread_mutex_t consume_mutex = PTHREAD_COND_INITIALIZER;
int consumed_elements = 0;;

/**
 * element_consume: creates a new element
 *
 * @producer: thread that produces it
 * @seed: this is needed for the random generator to work
 */
void element_consume(struct element *e)
{
	usleep(e->time);
	free(e);
	pthread_mutex_lock(&consume_mutex);
	consumed_elements++;
	pthread_mutex_unlock(&consume_mutex);
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
};

/**
 * queue_create: create a new queue
 *
 * @size: size of the queue
 */
static struct queue *queue_create(int size)
{
	struct queue *queue =  malloc(sizeof(struct queue));
	queue->size = size;
	queue->count = 0;
	queue->insertions = 0;
	queue->elements = malloc(sizeof(struct element) * size);

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
 * queue_insertions: returns the number of instertions
 *
 * @queue: queue to check
 */
static int queue_insertions(struct queue *queue)
{
	return queue->insertions;
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
	return queue->elements[queue->count];
}

pthread_cond_t buffer_full = PTHREAD_COND_INITIALIZER;
pthread_cond_t buffer_empty = PTHREAD_COND_INITIALIZER;

pthread_mutex_t buffer_mutex = PTHREAD_MUTEX_INITIALIZER;

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
};

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
		bool is_empty;
		struct element *e = element_create(args->thread_num, &seed);

		printf("%d: produces %d in %d microseconds\n", args->thread_num, e->value, e->time);
		pthread_mutex_lock(&buffer_mutex);

		while(queue_is_full(args->queue)) {
			pthread_cond_wait(&buffer_full, &buffer_mutex);
		}
		is_empty = queue_is_empty(args->queue);
		queue_insert(args->queue, e);
		if(is_empty)
			pthread_cond_broadcast(&buffer_empty);
		pthread_mutex_unlock(&buffer_mutex);
	}
	return NULL;
}

/**
 * consumer_args: Arguments passed to each consumer thread
 *
 * @thread_num: numerical identifier for our application
 * @queue: queue from where to remove elements
 */
struct consumer_args {
	int thread_num;
	struct queue *queue;
};

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
		struct element *e;
		bool is_full;
		pthread_mutex_lock(&buffer_mutex);
		while(queue_is_empty(args->queue))
			pthread_cond_wait(&buffer_empty, &buffer_mutex);
		is_full = queue_is_full(args->queue);
		e = queue_remove(args->queue);
		if(is_full)
			pthread_cond_broadcast(&buffer_full);
		pthread_mutex_unlock(&buffer_mutex);

		printf("%d: consumes %d in %d microseconds\n", args->thread_num, e->value, e->time);
		element_consume(e);
	}
	return NULL;
}

/**
 * producer_create: creates a new producer
 *
 * @info: thread information of created producer
 * @args: args passed to created thread
 * @thread_num: number id of created thread
 * @queue: where to insert producer elements
 */
void producer_create(struct thread_info *info, struct producer_args *args,
		     int thread_num, struct queue *queue)
{
	info->thread_num = thread_num;
	args->thread_num = thread_num;
	args->queue = queue;
	if ( 0 != pthread_create(&info->thread_id, NULL,
				 producer_function, args)) {
		printf("Failing creating consumer thread %d", thread_num);
		exit(1);
	}
}

/**
 * consumer_create: creates a new consumer
 *
 * @info: thread information of created consumer
 * @args: args passed to created thread
 * @thread_num: number id of created thread
 * @queue: where to remove elements
 */
void consumer_create(struct thread_info *info, struct consumer_args *args,
		     int thread_num, struct queue *queue)
{
	info->thread_num = thread_num;
	args->thread_num = thread_num;
	args->queue = queue;
	if ( 0 != pthread_create(&info->thread_id, NULL,
				 consumer_function, args)) {
		printf("Failing creating consumer thread %d", thread_num);
		exit(1);
	}
}

/**
 * producers_consumer: creates and wait to finish for producers and consumers
 *
 * @opt: command line options
 */
void producers_consumers(struct options *opt)
{
	int i;
	struct thread_info *producer_infos;
	struct thread_info *consumer_infos;
	struct producer_args *producer_args;
	struct consumer_args *consumer_args;
	struct queue *queue;

	printf("creating buffer with %d elements\n", opt->buffer_size);
	queue = queue_create(opt->buffer_size);

	if (queue == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	printf("creating %d producers\n", opt->num_producers);
	producer_infos = malloc(sizeof(struct thread_info) * opt->num_producers);

	if (producer_infos == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	producer_args = malloc(sizeof(struct producer_args) * opt->num_producers);

	if (producer_args == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	/* Create independent threads each of which will execute function */
	for (i = 0; i < opt->num_producers; i++) {
		producer_create(&producer_infos[i], &producer_args[i], i, queue);
	}

	printf("creating %d consumers\n", opt->num_consumers);
	consumer_infos = malloc(sizeof(struct thread_info) *opt-> num_consumers);

	if (consumer_infos == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	consumer_args = malloc(sizeof(struct consumer_args) * opt->num_consumers);

	if (consumer_args == NULL) {
		printf("Not enough memory\n");
		exit(1);
	}

	for (i = 0; i < opt->num_consumers; i++) {
		consumer_create(&consumer_infos[i], &consumer_args[i], i, queue);
	}
	for (i = 0; i < opt->num_producers; i++) {
		pthread_join(producer_infos[i].thread_id, NULL);
	}
	for (i = 0; i < opt->num_consumers; i++) {
		pthread_join(consumer_infos[i].thread_id, NULL);
	}
	free(producer_infos);
	free(consumer_infos);
	free(producer_args);
	free(consumer_args);
	queue_destroy(queue);
}

int main (int argc, char **argv)
{
	struct options opt;

	opt.num_producers = 2;
	opt.num_consumers = 2;
	opt.buffer_size = 1;
	opt.iterations = 100;

	read_options(argc, argv, &opt);

	producers_consumers(&opt);

	pthread_exit (0);
}


#include <getopt.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "options.h"

static struct option long_options[] = {
	{ .name = "producers",
	  .has_arg = required_argument,
	  .flag = NULL,
	  .val = 0},
	{ .name = "consumers",
	  .has_arg = required_argument,
	  .flag = NULL,
	  .val = 0},
	{ .name = "buffer_size",
	  .has_arg = required_argument,
	  .flag = NULL,
	  .val = 0},
	{ .name = "iterations",
	  .has_arg = required_argument,
	  .flag = NULL,
	  .val = 0},
	{0, 0, 0, 0}
};

static void usage(int i)
{
	printf(
		"Usage:  producers [OPTION] [DIR]\n"
		"Launch producers and consumers\n"
		"Opciones:\n"
		"  -p n, --producers=<n>: number of producers\n"
		"  -c n, --consumers=<n>: number of consumers\n"
		"  -b n, --buffer_size=<n>: number of elements in buffer\n"
		"  -i n, --iterations=<n>: total number of iterations\n"
		"  -h, --help: muestra esta ayuda\n\n"
	);
	exit(i);
}

static int get_int(char *arg, int *value)
{
	char *end;
	*value = strtol(arg, &end, 10);

	return (end != NULL);
}

static void handle_long_options(struct option option, char *arg, struct options *opt)
{
	if (!strcmp(option.name, "help"))
		usage(0);

	if (!strcmp(option.name, "producers")) {
		if (!get_int(arg, &opt->num_producers)
		    || opt->num_producers <= 0) {
			printf("'%s': no es un entero válido\n", arg);
			usage(-3);
		}
	}
	if (!strcmp(option.name, "consumers")) {
		if (!get_int(arg, &opt->num_consumers)
		    || opt->num_consumers <= 0) {
			printf("'%s': no es un entero válido\n", arg);
			usage(-3);
		}
	}
	if (!strcmp(option.name, "buffer_size")) {
		if (!get_int(arg, &opt->buffer_size)
		    || opt->buffer_size <= 0) {
			printf("'%s': no es un entero válido\n", arg);
			usage(-3);
		}
	}
	if (!strcmp(option.name, "iterations")) {
		if (!get_int(arg, &opt->iterations)
		    || opt->iterations <= 0) {
			printf("'%s': no es un entero válido\n", arg);
			usage(-3);
		}
	}
}

static int handle_options(int argc, char **argv, struct options *opt)
{
	while (1) {
		int c;
		int option_index = 0;

		c = getopt_long (argc, argv, "hp:c:b:i:",
				 long_options, &option_index);
		if (c == -1)
			break;

		switch (c) {
		case 0:
			handle_long_options(long_options[option_index],
					    optarg, opt);
			break;

		case 'p':
			if (!get_int(optarg, &opt->num_producers)
			    || opt->num_producers <= 0) {
				printf("'%s': no es un entero válido\n",
				       optarg);
				usage(-3);
			}
			break;


		case 'c':
			if (!get_int(optarg, &opt->num_consumers)
			    || opt->num_consumers <= 0) {
				printf("'%s': no es un entero válido\n",
				       optarg);
				usage(-3);
			}
			break;

		case 'b':
			if (!get_int(optarg, &opt->buffer_size)
			    || opt->buffer_size <= 0) {
				printf("'%s': no es un entero válido\n",
				       optarg);
				usage(-3);
			}
			break;

		case 'i':
			if (!get_int(optarg, &opt->iterations)
			    || opt->iterations <= 0) {
				printf("'%s': no es un entero válido\n",
				       optarg);
				usage(-3);
			}
			break;


		case '?':
		case 'h':
			usage(0);
			break;

		default:
			printf ("?? getopt returned character code 0%o ??\n", c);
			usage(-1);
		}
	}
	return 0;
}


int read_options(int argc, char **argv, struct options *opt) {

	int result = handle_options(argc, argv, opt);

	if (result != 0)
		exit(result);

	if (argc - optind != 0) {
		printf ("Extra arguments\n\n");
		while (optind < argc)
			printf ("'%s' ", argv[optind++]);
		printf ("\n");
		usage(-2);
	}

	return 0;
}

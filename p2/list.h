#ifndef __LIST_H__
#define __LIST_H__

typedef struct list *list;
typedef struct iterator* iterator;

list create_list(int max_size);
int insert_element(list l, void *e);
int remove_element(list l, void *e);
iterator get_iterator(list l);
void *get_element(iterator i);
void next(iterator it);
int is_end(iterator);
void free_iterator(iterator i);
void free_list(list l, void (*free_element)(void *));
#endif

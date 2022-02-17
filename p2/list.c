#include "list.h"
#include <stdlib.h>

struct list {
    int size;
    int n_elements;
    void **elements;
};

struct iterator {
    list l;
    int i;
};

list create_list(int max_size) {
    struct list *l=malloc(sizeof(struct list));
    
    l->size=max_size;
    l->elements=malloc(max_size*sizeof(void *));
    l->n_elements=0;
    
    int i;
    for(i=0; i<max_size; i++)
        l->elements[i]=NULL;
    
    return l;
}

int insert_element(list l, void *e) {
    int i;

    if(l->n_elements==l->size)
        return -1;
        
    l->n_elements++;
    
    for(i=0; i<l->size; i++)
        if(l->elements[i]==NULL) {
            l->elements[i]=e;
            break;
        }
    return 0;
}

int remove_element(list l, void *e) {
    int i;
    
    for(i=0;i<l->size; i++) 
        if(l->elements[i]==e) {
            l->elements[i]=NULL;
            return 0;
        }
    return -1;
}

iterator get_iterator(list l) {
    struct iterator *it=malloc(sizeof(struct iterator));
    
    it->l = l;
    it->i = 0;
    
    return it;
}

void *get_element(iterator it) {
    if(it->i>=it->l->n_elements)
        return NULL;
        
    int i, elem=0;
    for(i=0; elem<it->i && i<it->l->size; i++) 
        if(it->l->elements[i]!=NULL) elem++;
    
    if(elem==it->i) {
        return it->l->elements[elem];
    } else return NULL;
}

void next(iterator it) {
    it->i++;
}

int is_end(iterator it) {
    return it->i>=it->l->n_elements;
}

void free_iterator(iterator it) {
    free(it);
}

void free_list(list l, void (*free_element)(void *)) {
    int i;
    
    for(i=0; i<l->size; i++) {
        if(l->elements[i]!=NULL)
            free_element(l->elements[i]);
    }
    
    free(l->elements);
    free(l);
}

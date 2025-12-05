/** @file lists.c
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 04.02.2016 
 * @brief An array data structure
 * @version 1.1
*/

#include <stdlib.h>
#include <inttypes.h>

/**
 * @param list Pointer to the list
 * @param len_list Length of the list
 * @param size_of_entry Size of the entries
 * @param len_block The length of the block per new allocation. Never change!
 * @param alloc_fct The function to allocate memory for the entries
*/
void
alloc_list(void **list, const size_t len_list,
	   const size_t size_of_entry, const size_t len_block,
	   void (*alloc_fct) (void *, size_t, size_t))
{
    if (len_list == 0) {
	*list = malloc(len_block * size_of_entry);
	if (*list == NULL)
	    return;
	alloc_fct(*list, 0, len_block);
    } else {
	size_t tmp;
	if (SIZE_MAX - len_block * size_of_entry < len_list * size_of_entry) {
	    free(*list);
	    *list = NULL;
	    return;
	}
	tmp = (len_list + len_block);
	if (len_list % len_block == 0) {
	    *list = realloc(*list, tmp * size_of_entry);
	    if (*list == NULL)
		return;
	    alloc_fct(*list, len_list, tmp);
	}
    }
}

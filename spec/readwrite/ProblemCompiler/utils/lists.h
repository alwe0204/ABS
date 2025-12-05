/** @file lists.h
 * @author Alexander Weber (a.weber@unibw.de)
 * @date 04.02.2016 
 * @brief Header for lists.c
*/
#ifndef LISTS_H
#define LISTS_H

#define CURRENT_ENTRY(X) ((X)->list)[(X)->len]
#define UPDATE(X)  ((X)->len)++;

void alloc_list(void **list, const size_t len_list,
		const size_t size_of_entry, const size_t len_block,
		void (*alloc_fct) (void *, size_t, size_t));

#endif

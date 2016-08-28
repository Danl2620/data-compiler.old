/*
 * Copyright 2015 Dan Liebgold
 */

#include <new>
#include <memory>

#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <math.h>
#include <string.h>

//#define STB_DEFINE
//#include "stb.h"

#include "vm.h"
#include "string-utils.h"


const unsigned int kMagicFracas(FourCC<'F', 'r', 'a', 'c'>::value);

// ------------------------------------------------------------------------------------------------------------------ //
const void * align_pointer (const void * ptr, int align)
{
	return (const void*)((uintptr_t)ptr + (align - (uintptr_t)ptr % align));
}

// ------------------------------------------------------------------------------------------------------------------ //
void hex_dump (const void * buffer, int data_size)
{
	int lines = data_size/8 + 1;

	uint8_t * ptr = (uint8_t*)buffer;
	for (int ii = 0; ii < lines; ++ii)
	{
		printf("%016lx (%08x) : %02x %02x %02x %02x %02x %02x %02x %02x  ",
			   (uintptr_t)ptr, ii*8,
			   ptr[0], ptr[1], ptr[2], ptr[3],
			   ptr[4], ptr[5], ptr[6], ptr[7]);

#define CHAR(c) isalnum(c)?c:'.'
		printf("%c%c%c%c%c%c%c%c\n",
			   CHAR(ptr[0]), CHAR(ptr[1]), CHAR(ptr[2]), CHAR(ptr[3]),
			   CHAR(ptr[4]), CHAR(ptr[5]), CHAR(ptr[6]), CHAR(ptr[7]));
#undef CHAR
		ptr += 8;
	}
}


//
// read in the header,
// check header validity
// then read in the rest of the file, relying on buffering to even out performance
// pointers are on 8 byte boundaries, patch mask is probably best
//

// #define assert(val) \
// 	if (!(val))		\
// 	{ \
// 		printf("assert fails: " ##val "\n"); \
// 		exit(-1); \
// 	}

// ------------------------------------------------------------------------------------------------------------------ //
module_t::module_t (const module_t::header_t& hdr,
					const char * name,
					const void * buffer,
					const void * alloc_buffer)
	: m_header(hdr)
	, m_name(string::cache(name))
	, m_pointer(buffer)
	, m_alloc_buffer(alloc_buffer)
{}

// ------------------------------------------------------------------------------------------------------------------ //
module_t::~module_t ()
{
	free((void*)m_alloc_buffer);
}

// ------------------------------------------------------------------------------------------------------------------ //
void module_t::test1 () const
{
	printf("test1: 0x%016lx\n", m_start);
}

// ------------------------------------------------------------------------------------------------------------------ //
void module_t::test2 () const
{
	{
		const fracas::int32 * val = new ((void*)(base() + m_entries[0].m_offset)) fracas::int32;
		printf("0: %d\n", *val);
	}

	const fracas::string * str = new ((void*)(base() + m_entries[1].m_offset)) fracas::string;
	printf("1: '%s'/%d/%lu\n", str->c_str(), str->length(), strlen(str->c_str()));

	{
		const fracas::word64 * val = new ((void*)(base() + m_entries[2].m_offset)) fracas::word64;
		printf("2: 0x%016llx\n", *val);
	}
}

// ------------------------------------------------------------------------------------------------------------------ //
intptr_t module_t::base () const
{
	return m_start + m_header.m_count*sizeof(entry_t);
}

// ------------------------------------------------------------------------------------------------------------------ //
void module_t::debug_dump () const
{
	printf("contents of '%s':\n", m_name);
	hex_dump(m_pointer, m_header.m_size);

	printf("entries:\n");
	for (int ii = 0; ii < 3; ++ii)
	{
		printf("%d: %d\n", ii, m_entries[ii].m_symbol);
	}
}

// ------------------------------------------------------------------------------------------------------------------ //
uint32_t get_magic_code ()
{
	return kMagicFracas;
}

// ------------------------------------------------------------------------------------------------------------------ //
module_t * load_module (const char * module_name)
{
	char path[1024];
	snprintf(path, sizeof(path), "%s.bin", module_name);
	FILE * fp = fopen(path,"rb");
	if (!fp)
	{
		printf("Cannot open file (%d)\n", errno);
		return nullptr;
	}

	module_t::header_t header;
	if (1 != fread(&header, sizeof(module_t::header_t), 1, fp))
	{
		printf("Cannot read file (%d)\n", errno);
		fclose(fp);
		return nullptr;
	}

	size_t align = 16;
	size_t data_size = header.m_size;

	void * ptr = malloc(data_size + align);
	uint8_t * buffer = (uint8_t*)align_pointer(ptr, align);
	if (1 != fread(buffer, data_size, 1, fp))
	{
		printf("Error reading data (%d)\n", errno);
		free(ptr);
		fclose(fp);
		return nullptr;
	}
	return new module_t(header, module_name, buffer, ptr);
}

// ------------------------------------------------------------------------------------------------------------------ //
void free_module (module_t * module)
{
	delete module;
}

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
#include "stb.h"

#include "vm.h"
#include "string-utils.h"

template <int a, int b, int c, int d>
struct FourCC
{
    static const unsigned int value = (((((d << 8) | c) << 8) | b) << 8) | a;
};

const unsigned int kMagicDC(FourCC<'F', 'r', 'a', 'c'>::value);

// ------------------------------------------------------------------------------------------------------------------ //
const void * align_pointer (const void * ptr, int align)
{
	return (const void*)((uintptr_t)ptr + (align - (uintptr_t)ptr % align));
}

// ------------------------------------------------------------------------------------------------------------------ //
void hex_dump (const void * buffer, int data_size)
{
	int lines = data_size/8 + 1;

	uint8 * ptr = (uint8*)buffer;
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
class string_t
{
public:
	string_t() {}
	int length () const { return m_length; }
	const char * c_str () const { return m_string; }
private:
	int16_t m_length;
	char m_string[];
};



// ------------------------------------------------------------------------------------------------------------------ //
class module_t
{
	typedef uint32_t symbol_t;
	struct value_t
	{
		union
		{
			symbol_t m_symbol;
			int64_t m_integer;
		};
	};

	struct entry_t
	{
		symbol_t m_symbol;
		int32_t m_offset;
	};

public:
	module_t(const header_t& hdr,
			 const char * name,
			 const void * buffer,
			 const void * alloc_buffer);
	~module_t();
	const header_t& get_header() const;

	// debug
	void debug_dump() const;

	void test1 () const
	{
		const void * buffer = m_buffer;
		printf("test1: %d\n", *(int32_t*)buffer);
	}

	void test2 () const
	{
		const void * buffer = (const void*)((uintptr_t)m_buffer + sizeof(int32_t));
		printf("test2 (%lu):\n", *(uintptr_t*)buffer);
		printf("0x%016lx\n", (uintptr_t)buffer);
		const void * ptr = (const void *)((uintptr_t)buffer + *(uintptr_t*)buffer);
		printf("0x%016lx\n", (uintptr_t)ptr);
		string_t * str = (string_t*)ptr;
		printf("string test: '%s'/%d\n", str->c_str(), str->length());
	}

private:
	header_t m_header;
	const char * m_name;
	union
	{
		const void * m_buffer;
		const entry_t * m_entries;
	};
	const void * m_alloc_buffer;
};


// ------------------------------------------------------------------------------------------------------------------ //
module_t::module_t (const header_t& hdr,
					const char * name,
					const void * buffer,
					const void * alloc_buffer)
	: m_header(hdr)
	, m_name(string::cache(name))
	, m_buffer(buffer)
	, m_alloc_buffer(alloc_buffer)
{}

// ------------------------------------------------------------------------------------------------------------------ //
module_t::~module_t ()
{
	free((void*)m_alloc_buffer);
}

// ------------------------------------------------------------------------------------------------------------------ //
const header_t& module_t::get_header () const
{
	return m_header;
}

// ------------------------------------------------------------------------------------------------------------------ //
void module_t::debug_dump () const
{
	printf("contents of '%s':\n", m_name);
	hex_dump(m_buffer, m_header.m_size);

	printf("entries:\n");
	for (int ii = 0; ii < 3; ++ii)
	{
		printf("%d: %d\n", ii, m_entries[ii].m_symbol);
	}
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

	header_t header;
	if (1 != fread(&header, sizeof(header_t), 1, fp))
	{
		printf("Cannot read file (%d)\n", errno);
		fclose(fp);
		return nullptr;
	}

	size_t align = 16;
	size_t data_size = header.m_size;

	void * ptr = malloc(data_size + align);
	uint8 * buffer = (uint8*)align_pointer(ptr, align);
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

// ------------------------------------------------------------------------------------------------------------------ //
int main (int argc, const char * argv[])
{
	if (argc < 2)
		return -1;

	const char * module_name = argv[1];
	printf("loading module '%s',", module_name);
	module_t * module = load_module(module_name);

	const header_t& header = module->get_header();
	printf(" type: '%c%c%c%c', version: %d, size: %d, count: %d crc32: 0x%08x\n\n",
		   header.m_magic&255, (header.m_magic>>8)&255, (header.m_magic>>16)&255, (header.m_magic>>24)&255,
		   header.m_version,
		   header.m_size,
		   header.m_count,
		   header.m_crc32);

	assert(header.m_magic == kMagicDC);
	assert(header.m_version == 1);
	//assert(header.m_size > sizeof(header_t));

	module->debug_dump();
	//module->test1();
	//module->test2();

	// verify test entries
	// int32 * pI = (int32*)buffer;
	// assert(*pI == 24);

	// stb_uint64 * pU64 = (stb_uint64*)(buffer + 4);
	// assert(*pU64 == 0xffffffffffffffff);

	free_module(module);
	return 0;
}

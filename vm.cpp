
#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <math.h>
#include <memory>

//#define STB_DEFINE
#include "stb.h"

#include "vm.h"

template <int a, int b, int c, int d>
struct FourCC
{
    static const unsigned int value = (((((d << 8) | c) << 8) | b) << 8) | a;
};

const unsigned int kMagicDC(FourCC<'D', 'C', '0', '0'>::value);

void hex_dump (void * buffer, int data_size)
{
	int lines = data_size/8 + 1;

	uint8 * ptr = (uint8*)buffer;
	for (int ii = 0; ii < lines; ++ii)
	{
		printf("%016lx : %02x %02x %02x %02x %02x %02x %02x %02x\n",
			   (uintptr_t)ptr,
			   ptr[0], ptr[1], ptr[2], ptr[3],
			   ptr[4], ptr[5], ptr[6], ptr[7]);
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

int main ()
{
	FILE * fp = fopen("test.bin","rb");
	if (!fp)
	{
		printf("Cannot open file (%d)\n", errno);
		return -1;
	}

	header_t header;
	if (1 != fread(&header, sizeof(header_t), 1, fp))
	{
		printf("Cannot read file (%d)\n", errno);
		fclose(fp);
		return -1;
	}

	printf("header: \n  %d\n  %d\n  %d\n", header.m_magic, header.m_version, header.m_size);

	assert(header.m_magic == kMagicDC);
	assert(header.m_version == 1);
	assert(header.m_size > sizeof(header_t));

	size_t align = 16;
	size_t data_size = header.m_size - sizeof(header_t);
	size_t alloc_size = data_size + align;

	void * ptr = malloc(alloc_size);
	uint8 * buffer = (uint8*)((size_t)ptr + (align - (size_t)ptr % align));
	if (1 != fread(buffer, data_size, 1, fp))
	{
		printf("Error reading data (%d)\n", errno);
		fclose(fp);
		return -1;
	}

	hex_dump(buffer, data_size);

	// verify test entries
	int32 * pI = (int32*)buffer;
	assert(*pI == 24);

	stb_uint64 * pU64 = (stb_uint64*)(buffer + 4);
	assert(*pU64 == 0xffffffffffffffff);

	free(ptr);
	fclose(fp);

	return 0;
}


#include <stdio.h>
#include <errno.h>
#include <assert.h>
#include <math.h>

#include "stb.h"

#include "vm.h"

template <int a, int b, int c, int d>
struct FourCC
{
    static const unsigned int value = (((((d << 8) | c) << 8) | b) << 8) | a;
};

const unsigned int kMagicDC(FourCC<'D', 'C', '0', '0'>::value);


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
	//assert(fabs(header.m_value - 12.34) < 0.001);

	int32 data_size = header.m_size - sizeof(header_t);
	uint8 * buffer = (uint8*)malloc(data_size);
	if (1 != fread(buffer, data_size, 1, fp))
	{
		printf("Error reading data (%d)\n", errno);
		fclose(fp);
		return -1;
	}

	int32 * pI = (int32*)buffer;
	assert(*pI == 24);

	stb_uint64 * pU64 = (stb_uint64*)(buffer + 4);
	printf("pU64 == %lld\n", *pU64);
	assert(*pU64 == 0xffffffffffffffff);

	free(buffer);
	fclose(fp);

	return 0;
}

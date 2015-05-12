
#include <stdio.h>
#include <errno.h>

#include "stb.h"

#include "vm.h"

//
// read in the header,
// check header validity
// then read in the rest of the file, relying on buffering to even out performance
// pointers are on 8 byte boundaries, patch mask is probably best
//









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



	printf("header: \n  %d\n  %d\n  %f\n", header.m_magic, header.m_version, header.m_value);

	fclose(fp);
}

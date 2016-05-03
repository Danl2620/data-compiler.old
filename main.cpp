/*
 * Copyright 2016 Dan Liebgold
 */

#include <stdio.h>
#include <assert.h>

#include "vm.h"
#include "string-utils.h"

// ------------------------------------------------------------------------------------------------------------------ //
int main (int argc, const char * argv[])
{
	if (argc < 2)
		return -1;

	const char * module_name = argv[1];
	printf("loading module '%s',", module_name);
	module_t * module = load_module(module_name);

	const module_t::header_t& header = module->get_header();
	printf(" type: '%c%c%c%c', version: %d, size: %d, count: %d crc32: 0x%08x\n\n",
		   header.m_magic&255, (header.m_magic>>8)&255, (header.m_magic>>16)&255, (header.m_magic>>24)&255,
		   header.m_version,
		   header.m_size,
		   header.m_count,
		   header.m_crc32);

	assert(header.m_magic == get_magic_code());
	assert(header.m_version == 1);
	//assert(header.m_size > sizeof(header_t));

	module->debug_dump();
	module->test1();
	module->test2();

	// verify test entries
	// int32 * pI = (int32*)buffer;
	// assert(*pI == 24);

	// stb_uint64 * pU64 = (stb_uint64*)(buffer + 4);
	// assert(*pU64 == 0xffffffffffffffff);

	free_module(module);
	return 0;
}

/*
 * Copyright 2015 Dan Liebgold
 */

#include <memory>
#include <assert.h>

#include "stb.h"
#include "string-utils.h"


// ------------------------------------------------------------------------------------------------------------------ //
//  a simple string cache
// ------------------------------------------------------------------------------------------------------------------ //
static char s_cache_buffer[1024];
static const char * s_cache_strings[64];
static int16_t s_cache_lengths[64];
static int s_cache_index;

namespace string
{
const char * cache (const char * str)
{
	for (int ii = 0; ii < s_cache_index; ++ii)
	{
		if (0 == strcmp(str, s_cache_strings[ii]))
			return s_cache_strings[ii];
	}

	char * nextstr;
	if (s_cache_index == 0)
	{
		s_cache_strings[0] = nextstr = s_cache_buffer;
	}
	else
	{
		nextstr = const_cast<char*>(s_cache_strings[s_cache_index-1] + s_cache_lengths[s_cache_index-1] + 1);
	}

	int len = strlen(str);
	int remaining = (sizeof(s_cache_buffer) - (nextstr - s_cache_buffer));
	assert(len < remaining);
	if (len >= remaining)
		return nullptr;

	strncpy(nextstr, str, remaining);
	nextstr[remaining-1] = 0;
	s_cache_strings[s_cache_index] = nextstr;
	s_cache_lengths[s_cache_index] = len;
	s_cache_index++;
	return nextstr;
}
}

/*
 * Copyright 2015 Dan Liebgold
 */

#ifndef VM_H
#define VM_H

struct header_t
{
	uint32 m_magic;
	int32 m_version;
	int32 m_size;
	int32 m_count;
	uint32 m_crc32;
};

#endif // VM_H

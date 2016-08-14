/*
 * Copyright 2015 Dan Liebgold
 */

#ifndef VM_H
#define VM_H

#include <stdint.h>

template <int a, int b, int c, int d>
struct FourCC
{
    static const uint32_t value = (((((d << 8) | c) << 8) | b) << 8) | a;
};

// --------------------------------------------------------------------------- //
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

// --------------------------------------------------------------------------- //
namespace fracas
{
	typedef int32_t int32;
	typedef int64_t int64;
	typedef uint32_t word32;
	typedef uint64_t word64;
	typedef string_t string;
	typedef uint32_t symbol;
};

// ------------------------------------------------------------------------------------------------------------------ //
class module_t
{
public:
	struct header_t
	{
		uint32_t m_magic;
		int32_t m_version;
		int32_t m_size;
		int32_t m_count;
		uint32_t m_crc32;
	};

	module_t(const header_t& hdr,
			 const char * name,
			 const void * buffer,
			 const void * alloc_buffer);
	virtual ~module_t();
	const header_t& get_header () const { return m_header; }

	// debug
	void debug_dump() const;
	void test1 () const;
	void test2 () const;

private:
	struct entry_t
	{
		fracas::symbol m_symbol;
		int32_t m_offset;
	};

	intptr_t base () const;
	const char * m_name;
	header_t m_header;
	union
	{
		const entry_t * m_entries;
		intptr_t m_start;
		const void * m_pointer;
	};
	const void * m_alloc_buffer;
};

extern uint32_t get_magic_code();
extern module_t * load_module(const char * module_name);
extern void free_module(module_t * module);

#endif // VM_H

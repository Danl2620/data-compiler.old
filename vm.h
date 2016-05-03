/*
 * Copyright 2015 Dan Liebgold
 */

#ifndef VM_H
#define VM_H

// --------------------------------------------------------------------------- //
struct header_t
{
	uint32 m_magic;
	int32 m_version;
	int32 m_size;
	int32 m_count;
	uint32 m_crc32;
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
namespace dc
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
	struct entry_t
	{
		dc::symbol m_symbol;
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
	void test1 () const;
	void test2 () const;

	intptr_t base () const;
private:
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

#endif // VM_H

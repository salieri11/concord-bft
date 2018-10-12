// Copyright 2018 VMware, all rights reserved

/**
 * Blockchain::Sliver -- Zero-copy management of bytes.
 *
 * Sliver provides a view into an allocated region of memory. Views of
 * sub-regions, or "sub-slivers" do not copy data, but instead reference the
 * memory of the "base" sliver. Any sub-sliver can be considered the base sliver
 * for further sub-slivers.
 *
 * The memory is managed through a std::shared_ptr. If the `Sliver(char* data,
 * size_t length)` constructor is called, that sliver wraps the data pointer in
 * a shared pointer. Sub-slivers reference this same shared pointer, such that
 * the memory is kept around as long as the base sliver or any sub-sliver needs
 * it, and cleaned up once the base sliver and all sub-slivers have finished
 * using it.
 */

#include <cassert>
#include <memory>

#include "sliver.hpp"

/**
 * Create a new sliver that will own the memory pointed to by `data`, which is
 * `length` bytes in size.
 */
Blockchain::Sliver::Sliver(uint8_t* data, const size_t length)
  : m_data(data, free), m_offset(0), m_length(length) {
  // Data must be non-null.
  assert(data);
}

/**
 * Create a sub-sliver that references a region of a base sliver.
 */
Blockchain::Sliver::Sliver(const Sliver& base, const size_t offset, const size_t length)
  : m_data(base.m_data),
    // This sliver starts offset bytes from the offset of its base.
    m_offset(base.m_offset + offset),
    m_length(length) {
  // This sliver must start no later than the end of the base sliver.
  assert(offset <= base.m_length);
  // This sliver must end no later than the end of the base sliver.
  assert(length <= base.m_length - offset);
}

/**
 * Get the byte at `offset` in this sliver.
 */
uint8_t Blockchain::Sliver::operator[](const size_t offset) const {
  // This offset must be within this sliver.
  assert(offset < m_length);

  // The data for the requested offset is that many bytes after the offset from
  // the base sliver.
  return m_data.get()[m_offset+offset];
}

/**
 * Get a direct pointer to the data for this sliver. Remember that the Sliver
 * (or its base) still owns the data, so ensure that the lifetime of this Sliver
 * (or its base) is at least as long as the lifetime of the returned pointer.
 */
uint8_t* Blockchain::Sliver::data() const {
  return m_data.get()+m_offset;
}

/**
 * Create a subsliver. Syntactic sugar for cases where a function call is more
 * natural than using the sub-sliver constructor directly.
 */
Blockchain::Sliver Blockchain::Sliver::subsliver(const size_t offset,
                                                 const size_t length) const {
  return Blockchain::Sliver(*this, offset, length);
}

size_t Blockchain::Sliver::length() const {
  return m_length;
}

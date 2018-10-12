// Copyright 2018 VMware, all rights reserved

/**
 * Blockchain::Sliver -- Zero-copy management of bytes.
 *
 * Sliver provides a view into an allocated region of memory. Views of
 * sub-regions, or "sub-slivers" do not copy data, but instead reference the
 * memory of the "base" sliver.
 *
 * The memory is managed through a std::shared_ptr. If the `Sliver(char* data,
 * size_t length)` constructor is called, that sliver wraps the data pointer in
 * a shared pointer. Sub-slivers reference this same shared pointer, such that
 * the memory is kept around as long as the base sliver or any sub-sliver needs
 * it, and cleaned up once the base sliver and all sub-slivers have finished
 * using it.
 */

#ifndef SLIVER_HPP
#define SLIVER_HPP

#include <memory>

namespace Blockchain {
class Sliver {
 public:
  Sliver(uint8_t* data, const size_t length);
  Sliver(const Sliver& base, const size_t offset, const size_t length);

  uint8_t operator[](const size_t offset) const;

  Sliver subsliver(const size_t offset, const size_t length) const;

  size_t length() const;
  uint8_t* data() const;

 private:
  // these are never modified, but need to be non-const to support copy
  // assignment
  std::shared_ptr<uint8_t> m_data;
  size_t m_offset;
  size_t m_length;
};
}

#endif  // SLIVER_HPP

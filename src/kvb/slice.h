// Copyright (c) 2011 The LevelDB Authors. All rights reserved.
// Use of this source code is governed by a BSD-style license that can be
// found in the LICENSE file. See the AUTHORS file for names of contributors.
//
// Slice is a simple structure containing a pointer into some external
// storage and a size.  The user of a Slice must ensure that the slice
// is not used after the corresponding external storage has been
// deallocated.
//
// Multiple threads can invoke const methods on a Slice without
// external synchronization, but if any of the threads may call a
// non-const method, all threads accessing the same Slice must use
// external synchronization.

// TODO(BWF): Given somee very different formattings in some place, I believe
// that some of the things in this file are *not* LevelDB Author originals, like
// toBytes, overlapsWith, copyTo, copyToAndAdvance, copyFromAndAdvance, and
// operator<

#ifndef STORAGE_LEVELDB_INCLUDE_SLICE_H_
#define STORAGE_LEVELDB_INCLUDE_SLICE_H_

#include <assert.h>
#include <stddef.h>
#include <string.h>
#include <string>
#include <stdio.h>

#include <vector>
#include <algorithm>

namespace Blockchain {

class Slice {
 public:
  // Create an empty slice.
  Slice() : data_(""), size_(0) { }

  // Create a slice that refers to d[0,n-1].
  Slice(const char* d, size_t n) : data_(d), size_(n) { }

  // Create a slice that refers to the contents of "s"
  Slice(const std::string& s) : data_(s.data()), size_(s.size()) { }

  // Create a slice that refers to s[0,strlen(s)-1]
  Slice(const char* s) : data_(s), size_(strlen(s)) { }

  // Return a pointer to the beginning of the referenced data
  const char* data() const { return data_; }

  // Return the length (in bytes) of the referenced data
  size_t size() const { return size_; }

  // Return true iff the length of the referenced data is zero
  bool empty() const { return size_ == 0; }

  // Return the ith byte in the referenced data.
  // REQUIRES: n < size()
  char operator[](size_t n) const {
    assert(n < size());
    return data_[n];
  }

  // Change this slice to refer to an empty array
  void clear() { data_ = ""; size_ = 0; }

  // Drop the first "n" bytes from this slice.
  void remove_prefix(size_t n) {
    assert(n <= size());
    data_ += n;
    size_ -= n;
  }

  // Return a string that contains the copy of the referenced data.
  std::string ToString() const { return std::string(data_, size_); }

  // Three-way comparison.  Returns value:
  //   <  0 iff "*this" <  "b",
  //   == 0 iff "*this" == "b",
  //   >  0 iff "*this" >  "b"
  int compare(const Slice& b) const;

  // Return true iff "x" is a prefix of "*this"
  bool starts_with(const Slice& x) const {
    return ((size_ >= x.size_) &&
            (memcmp(data_, x.data_, x.size_) == 0));
  }

  inline std::vector<unsigned char> toBytes() const
  {
     return std::vector<unsigned char>(this->data(),
                                       this->data() + this->size());
  }

  bool overlapsWith(Slice& _t) const
  {
     void const* f1 = data();
     void const* t1 = data() + size();
     void const* f2 = _t.data();
     void const* t2 = _t.data() + _t.size();
     return f1 < t2 && t1 > f2;

  }
  /// Copies the contents of this vector_ref to the contents of @a _t, up to the
  /// max size of @a _t.
  void copyTo(Slice& _t) const
  {
     if (overlapsWith(_t)) {
        memmove((void*)_t.data(),
                (void*)this->data(),
                std::min(_t.size(), this->size()));
     } else {
        memcpy((void*)_t.data(),
               (void*)this->data(),
               std::min(_t.size(), this->size()));
     }
  }

 private:
  const char* data_;
  size_t size_;

  // Intentionally copyable
};

inline bool operator<(const Slice& x, const Slice& y)
{
   size_t ys = y.size();
   if(ys == 0) {
      return false;
   }
   size_t xs = x.size();
   if(xs == 0) {
      return true;
   }
   size_t mins = (xs<ys ? xs : ys);
   return memcmp(x.data(),y.data(),mins) < 0;
}

inline bool operator==(const Slice& x, const Slice& y) {
  return ((x.size() == y.size()) &&
          (memcmp(x.data(), y.data(), x.size()) == 0));
}

inline bool operator!=(const Slice& x, const Slice& y) {
  return !(x == y);
}

inline int Slice::compare(const Slice& b) const {
  const size_t min_len = (size_ < b.size_) ? size_ : b.size_;
  int r = memcmp(data_, b.data_, min_len);
  if (r == 0) {
    if (size_ < b.size_) r = -1;
    else if (size_ > b.size_) r = +1;
  }
  return r;
}

Slice append(const Slice& out, const Slice& s);

bool copyToAndAdvance(char *_buf,
                      size_t *_offset,
                      size_t _maxOffset,
                      char *_src,
                      size_t _srcSize);
bool copyFromAndAdvance(const char *_buf,
                        size_t *_offset,
                        size_t _maxOffset,
                        char *_to,
                        size_t _toSize);

}  // namespace Blockchain

#endif // STORAGE_LEVELDB_INCLUDE_SLICE_H_

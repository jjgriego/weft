#pragma once

#include "stdint.hpp"
#include "optional.hpp"

namespace bytes {

struct bytes_view {
  optional<size_t> find(uint8_t, size_t start = 0) const;

  const uint8_t* cbegin() const { return ptr; }
  const uint8_t* cend() const { return ptr + len; }

  uint8_t operator[](size_t idx) const { return ptr[idx]; }

  bytes_view slice(size_t start, size_t end) const;

  size_t size() const { return len; }

  const uint8_t* ptr;
  size_t len;
};

bytes_view operator ""_bv (const char* ptr, size_t len);

}

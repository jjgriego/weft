#pragma once

#include "stdint.hpp"
#include "optional.hpp"

namespace bytes {

struct bytes_view {
  optional<size_t> find(uint8_t, size_t start = 0) const;

  const uint8_t* ptr;
  size_t len;
};

bytes_view operator ""_bv (const char* ptr, size_t len);

}

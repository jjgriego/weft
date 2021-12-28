#include "bytes.hpp"
#include "panic.hpp"

namespace bytes {

bytes_view operator ""_bv (const char* ptr, size_t len) {
  return bytes_view {reinterpret_cast<const uint8_t*>(ptr), len};
}

optional<size_t> bytes_view::find(uint8_t needle, size_t start /* = 0 */) const {
  for (size_t off = start; off < len; off++) {
    if (ptr[off] == needle) {
      return off;
    }
  }
  return {};
}

bytes_view bytes_view::slice(size_t begin, size_t end) const {
  if (end < begin) panic("bad slice"_bv);
  return bytes_view { ptr + begin, end - begin};
}

}

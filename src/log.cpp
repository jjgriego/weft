#include "log.hpp"

namespace log_detail {

namespace {

uint8_t nibble_char(uint8_t nibble) {
  if (nibble < 10) {
    return nibble + '0';
  } else {
    return (nibble - 10) + 'a';
  }
}

}

void put(uint64_t i) {
  constexpr uint64_t nibble_mask = 0x0f;
  uint8_t nibbles[16];

  const auto& extract_nibble = [&]() {
    const auto result = i & nibble_mask;
    i = i >> 4;
    return static_cast<uint8_t>(result);
  };

  for (auto n = 15; n >= 0; n--) {
    nibbles[n] = nibble_char(extract_nibble());
  }

  serial::write(bytes::bytes_view{ nibbles, 16 });
}

void put(bytes::bytes_view bv) {
  serial::write(bv);
}

}

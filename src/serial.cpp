#include "serial.hpp"

#include "stdint.hpp"

using namespace bytes;

namespace serial {

inline uint8_t inb(uint16_t port) {
  uint8_t out;
  asm volatile ("inb %%dx, %%al"
                : "=a" (out)
                : "d" (port));
  return out;
}

inline void outb(uint16_t port, uint8_t val) {
  asm volatile ("outb %%al, %%dx"
                : /* no outputs */
                : "a" (val), "d" (port));
}

inline void write_str_port(uint16_t port, const uint8_t* bytes, size_t len) {
  for (size_t i = 0; i < len; i++) {
    while ((inb(port + 5) & 0x20) == 0);
    outb(port, bytes[i]);
  }
}

void write(bytes_view bv) {
  write_str_port(0x3f8, bv.ptr, bv.len);
}

}


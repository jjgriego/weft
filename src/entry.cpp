export module entry;

import stdint;
import bootboot;
import virtmem;


using size_t = uint64_t;

inline uint8_t inb(uint16_t port) {
  uint8_t out;
  asm volatile ("inb %%dx, %%al"
                : "=a" (out)
                : "d" (port));
  return out;
}

#define outb(port, val)                         \
  asm volatile ("outb %%al, %%dx"               \
                : /* no outputs */              \
                : "a" (val), "d" (port))


inline void write_str_port(uint16_t port, const char* bytes, size_t len) {
  for (size_t i = 0; i < len; i++) {
    while ((inb(port + 5) & 0x20) == 0);
    outb(port, bytes[i]);
  }
}

extern "C" {

extern unsigned char environment[4096];
extern uint8_t fb;

[[noreturn]]
void entry_point()
{
  write_str_port(0x3f8, "hello, cruel world\r\n", 20);
  const auto mmap_entries = bootboot.size;
  auto entry = &bootboot.mmap;
  for (size_t i = 0; i < mmap_entries; i++) {
    write_str_port(0x3f8, "entry\r\n", 7);
  }
  while (1);
}

}


#include <cstdint>
#include <bootboot.h>

using size_t = uint64_t;

inline uint8_t inb(uint16_t port) {
  uint8_t out;
  asm volatile ("movw %1, %%dx \n\t"
                "inb %%dx, %%al \n\t"
                "mov %%al, %0 \n\t"
                : "=r" (out)
                : "g" (port)
                : "rax", "dx");
  return out;
}

#define outb(port, val)                         \
  asm volatile ("movb %0, %%al\n\t"             \
                "movw %1, %%dx\n\t"             \
                "outb %%al, %%dx"               \
                : /* no outputs */              \
                : "g" (val), "n" (port)         \
                : "rax", "dx")                  \


inline void write_str_port(uint16_t port, const char* bytes, size_t len) {
  for (size_t i = 0; i < len; i++) {
    while ((inb(port + 5) & 0x20) == 0);
    outb(port, bytes[i]);
  }
}

extern "C" {

extern BOOTBOOT bootboot;
extern unsigned char environment[4096];
extern uint8_t fb;

[[noreturn]]
void entry_point()
{
  write_str_port(0x3f8, "hello, cruel world\r\n", 20);
  while (1);
}

}


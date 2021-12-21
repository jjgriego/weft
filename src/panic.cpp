#include "panic.hpp"

#include "bytes.hpp"
#include "serial.hpp"

using namespace bytes;

[[noreturn]]
void panic(bytes_view msg) {
  serial::write("panic: "_bv);
  serial::write(msg);
  serial::write("\r\n(ya silly goose)\r\n"_bv);
  // we should triple-fault, but for now, just hang!
  while(1);
}

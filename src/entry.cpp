export module entry;

import stdint;
import bootboot;
import virtmem;
import serial;
import bytes;
import panic;
import optional;

using namespace bytes;

extern "C" {

extern unsigned char environment[4096];
extern uint8_t fb;

}

[[noreturn]]
void entry_point() {
  optional<uint64_t> o;
  serial::write("hello, cruel world\r\n"_bv);
  virtmem::init();
  auto i = *o;
  panic("end of entry point reached"_bv);
}


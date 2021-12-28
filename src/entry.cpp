#include "entry.hpp"

#include "allocator.hpp"
#include "bootboot.hpp"
#include "bytes.hpp"
#include "log.hpp"
#include "panic.hpp"
#include "serial.hpp"
#include "virtmem.hpp"
#include "system.hpp"

using namespace bytes;

extern "C" {

extern unsigned char environment[4096];
extern uint8_t fb;

[[noreturn]]
void entry_point() {
  bootboot_util::display_configuration();
  virtmem::init();
  auto const heap = virtmem::allocator_region();

  (*system::gdtr()).table().dump();

  log("% bytes available for malloc"_bv, heap.size());
  allocator::init(move(heap));
  panic("end of entry point reached"_bv);
}


}


#pragma once

#include "stdint.hpp"
#include "serial.hpp"

/*
 * This module is responsible for tracking the virtual memory state of the
 * machine, which, for now, is a relatively simple task since we're not going to
 * change the memory map we get handed from the bootloader right now.
 *
 * This is also the source of truth for the allocator and other parts of the
 * runtime to reserve pages of memory
 */

namespace virtmem {

void init();

struct page_handle {
  uintptr_t base_addr;
  size_t    length;
};

page_handle reserve_page();



}

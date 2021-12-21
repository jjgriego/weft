export module virtmem;

import bootboot;
import stdint;
import serial;
import bytes;
import panic;

using namespace bytes;

/*
 * This module is responsible for tracking the virtual memory state of the
 * machine, which, for now, is a relatively simple task since we're not going to
 * change the memory map we get handed from the bootloader right now.
 *
 * This is also the source of truth for the allocator and other parts of the
 * runtime to reserve pages of memory
 */

namespace virtmem {

constexpr size_t mmap_max_entries = 256;

struct map {
  enum class entry_type {
    available, system
  };
  struct entry {
    entry_type type;
    uintptr_t base;
    size_t size;
  };

  entry entries[mmap_max_entries];
} g_map;

export void init() {
  serial::write("initializing virtmem\r\n"_bv);
  using namespace bootboot_util;

  const auto mmap_entries = bootboot.size;
  auto entry = &bootboot.mmap;
  if (mmap_entries > mmap_max_entries) {
    panic("too many mmap entries at boot to fit in table"_bv);
  }
  for (size_t i = 0; i < mmap_entries; i++, entry++) {
    auto type = mmap_entry_type(entry);
    switch (type) {
    case entry_used:
    case entry_acpi:
    case entry_mmio:
      g_map.entries[i] = map::entry {
        map::entry_type::system,
        entry->ptr,
        mmap_entry_size(entry)
      };
      break;
    case entry_free:
      g_map.entries[i] = map::entry {
        map::entry_type::available,
        entry->ptr,
        mmap_entry_size(entry)
      };
      break;
    default:
      break;
    }
  }
}

export struct page_handle {
  uintptr_t base_addr;
  size_t    length;
};

page_handle reserve_page() {

  
}

}


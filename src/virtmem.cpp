#include "virtmem.hpp"

#include "bootboot.hpp"
#include "bytes.hpp"
#include "serial.hpp"
#include "panic.hpp"

using namespace bytes;

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

void init() {
  serial::write("initializing virtmem\r\n"_bv);

  const auto mmap_entries = bootboot.size;
  auto entry = &bootboot.mmap;
  if (mmap_entries > mmap_max_entries) {
    panic("too many mmap entries at boot to fit in table"_bv);
  }
  for (size_t i = 0; i < mmap_entries; i++, entry++) {
    auto type = MMapEnt_Type(entry);
    switch (type) {
    case MMAP_USED:
    case MMAP_ACPI:
    case MMAP_MMIO:
      g_map.entries[i] = map::entry {
        map::entry_type::system,
        entry->ptr,
        MMapEnt_Size(entry)
      };
      break;
    case MMAP_FREE:
      g_map.entries[i] = map::entry {
        map::entry_type::available,
        entry->ptr,
        MMapEnt_Size(entry)
      };
      break;
    default:
      break;
    }
  }
}

page_handle reserve_page() {
  panic("nyi"_bv);

  
}

}


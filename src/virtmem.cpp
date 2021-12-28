#include "virtmem.hpp"

#include "bootboot.hpp"
#include "bytes.hpp"
#include "serial.hpp"
#include "panic.hpp"
#include "log.hpp"

using namespace bytes;

namespace virtmem {

namespace {

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

  size_t entry_count;
  entry entries[mmap_max_entries];
} g_map;

uintptr_t read_cr3() {
  uintptr_t r = 0;
  asm volatile ( "mov %%cr3, %0"
               : "=r" (r));
  return r;
}

vregion g_alloc_region;

}

void init() {
  serial::write("initializing virtmem\r\n"_bv);

  g_map.entry_count = 0;

  const auto mmap_entries = (bootboot.size - 128) / 16;
  auto entry = &bootboot.mmap;
  if (mmap_entries > mmap_max_entries) {
    panic("too many mmap entries at boot to fit in table"_bv);
  }
  for (size_t i = 0; i < mmap_entries; i++, entry++) {
    auto type = MMapEnt_Type(entry);
    switch (type) {
    default:
    case MMAP_USED:
    case MMAP_ACPI:
    case MMAP_MMIO:
      g_map.entries[i] = map::entry {
        map::entry_type::system,
        entry->ptr,
        MMapEnt_Size(entry)
      };
      g_map.entry_count++;
      break;
    case MMAP_FREE:
      g_map.entries[i] = map::entry {
        map::entry_type::available,
        entry->ptr,
        MMapEnt_Size(entry)
      };
      g_map.entry_count++;
      break;
    }
  }

  for (size_t i = 0; i < g_map.entry_count; i++) {
    const auto& ent = g_map.entries[i];
    const auto label = ent.type == map::entry_type::available ? "avail"_bv : "sys"_bv;
    log("mmap ent % - % (% bytes) %"_bv, ent.base, (ent.base + ent.size - 1), ent.size, label);
  }

  // find the largest region of free memory
  size_t largest_idx = 0;
  size_t largest_size = 0;
  for (size_t i = 0; i < g_map.entry_count; i++) {
    const auto& ent = g_map.entries[i];
    if (ent.size > largest_size) {
      largest_idx = i;
      largest_size = ent.size;
    }
  }

  const auto& largest = g_map.entries[largest_idx];
  // round the base up to the nearest 4kb and the size down to the nearest 4kb
  size_t base = (largest.base + 0xfffUL) & (~0xfffUL);
  size_t size = (largest.size + largest.base - base) & ~0xfffUL;
  size_t pages_size = size / 0x1000;
  log("allocation region chosen as % - % (% x 4k)"_bv, base, base + size, pages_size);
  g_alloc_region = vregion { base, size };

  log("cr3 = %"_bv, read_cr3());
}

vregion allocator_region() {
  return g_alloc_region;
}



}


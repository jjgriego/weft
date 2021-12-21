export module bootboot;

import stdint;

export {

#include <bootboot.h>

extern "C" {
  extern BOOTBOOT bootboot;
}

namespace bootboot_util {

inline uint64_t mmap_entry_size(MMapEnt* ent) {
  return MMapEnt_Size(ent);
}

inline constexpr uint64_t entry_used = MMAP_USED;
inline constexpr uint64_t entry_free = MMAP_FREE;
inline constexpr uint64_t entry_acpi = MMAP_ACPI;
inline constexpr uint64_t entry_mmio = MMAP_MMIO;

inline uint64_t mmap_entry_type(MMapEnt* ent) {
  return MMapEnt_Type(ent);
}


  

}

}

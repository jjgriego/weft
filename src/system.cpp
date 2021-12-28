#include "system.hpp"

#include "log.hpp"

namespace system{

using namespace bytes;

void segment_desc_table::dump() const {
  log("segment table (% entries)"_bv, size);
  for (size_t i = 0; i < size; i++) {
    const auto& entry = entries[i];

    const uint64_t base =
      entry.m_base0
      + uint64_t(entry.m_base1 << 16)
      + uint64_t(entry.m_base2 << 24);
    const uint64_t limit = entry.m_limit0 + uint64_t(entry.m_limit1 << 16);

    if (entry.m_present) {
      log("  % %"_bv, base, limit);
    } else {
      log("  <not present>"_bv);
    }
  }
}


}


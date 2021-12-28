#pragma once

#include "stdint.hpp"
#include "util.hpp"

namespace system {

struct segment_desc_table {
  /* see intel SDM vol 3 §3.4.5 */
  struct entry {
    uint16_t m_limit0       : 16;
    uint32_t m_base0        : 16;
    uint16_t m_base1        : 8;
    uint8_t  m_type         : 4;
    uint8_t  m_desc_type    : 1;
    uint8_t  m_priv         : 2;
    uint8_t  m_present      : 1;
    uint8_t  m_limit1       : 4;
    uint8_t  m_sys_avail    : 1;
    uint8_t  m_64b          : 1;
    uint8_t  m_default_size : 1;
    uint8_t  m_granularity  : 1;
    uint8_t  m_base2        : 8;
  };
  static_assert(sizeof(entry) == sizeof(char[8]));

  void dump() const;

  // size includes the empty entry at the start of the table
  size_t size;
  entry* entries;
};


struct alignas(uint64_t) gdtr_value {
  operator uint64_t() const {
    return *reinterpret_cast<const uint64_t*>(this);
  }

  segment_desc_table table() const {
    return {limit, reinterpret_cast<segment_desc_table::entry*>(base)};
  }

  uint16_t limit : 16;
  uint64_t base : 48;
};

static_assert(sizeof(gdtr_value) == sizeof(uint64_t));
static_assert(alignof(gdtr_value) >= alignof(uint64_t));

struct gdtr_reg {
  gdtr_value operator*() const {
    gdtr_value ret;
    asm volatile ("sgdt %0"
                  : "=m" (ret));
    return ret;
  }

  operator gdtr_value() const {
    return **this;
  }


};

inline gdtr_reg gdtr() { return {}; }



}


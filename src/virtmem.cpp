export module virtmem;
/*

import bootboot;
import stdint;

namespace virtmem {

export void init(const BOOTBOOT& boot_config) {
  

}

export struct page_handle {

  uintptr_t base_addr() const;
  size_t size() const;

private:
  uintptr_t m_base;
  size_t m_size;
};

/*
 * We probably want to be way more sophisticated about this but for the time
 * being it's probably fine to just grab pages in the id-mapped region we get at
 * boot and just panic if we run out
 * /
export page_handle alloc_page() {

  return page_handle { 0xf00f, 0xbabe };
}



*/

#include "bootboot.hpp"

#include "log.hpp"
#include "bytes.hpp"

using namespace bytes;

namespace bootboot_util {

void display_configuration() {
  log("========================================"_bv);
  log("configuration from bootloader"_bv);
  log(""_bv);
  log("bootboot params total size: %"_bv, bootboot.size);
  log(" bootboot protocol version: %"_bv, bootboot.protocol);
  log("          framebuffer type: %"_bv, bootboot.fb_type);
  log("                     bspid: %"_bv, bootboot.bspid);
  log("               initrd base: %"_bv, bootboot.initrd_ptr);
  log("               initrd size: %"_bv, bootboot.initrd_size);
  log("                   fb base: %"_bv, bootboot.fb_ptr);
  log("                   fb size: %"_bv, bootboot.fb_size);
  log("                 acpi base: %"_bv, bootboot.arch.x86_64.acpi_ptr);
  log("                 smbi base: %"_bv, bootboot.arch.x86_64.smbi_ptr);
  log("                  efi base: %"_bv, bootboot.arch.x86_64.efi_ptr);
  log("                   mp base: %"_bv, bootboot.arch.x86_64.mp_ptr);
}


}

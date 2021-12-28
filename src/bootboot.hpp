#pragma once

#include "stdint.hpp"
#include <bootboot.h>

extern "C" {
  extern BOOTBOOT bootboot;
}

namespace bootboot_util {

void display_configuration();

}

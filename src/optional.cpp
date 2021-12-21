#include "optional.hpp"
#include "bytes.hpp"
#include "panic.hpp"

namespace optional_detail {

using namespace bytes;

[[noreturn]]
void panic_missing_optional() {
  panic("missing optional required"_bv);
}

}

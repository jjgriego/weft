#pragma once

#include "bytes.hpp"

[[noreturn]] void panic(bytes::bytes_view msg);

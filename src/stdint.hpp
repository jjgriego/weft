#pragma once

using uint8_t = unsigned char;
using uint16_t = unsigned short;
using uint32_t = unsigned int;
using uint64_t = unsigned long;

using int8_t = signed char;
using int16_t = signed short;
using int32_t = signed int;
using int64_t = signed long;

static_assert(sizeof(uint8_t) == 1);
static_assert(sizeof(uint16_t) == 2);
static_assert(sizeof(uint32_t) == 4);
static_assert(sizeof(uint64_t) == 8);
static_assert(sizeof(int8_t) == 1);
static_assert(sizeof(int16_t) == 2);
static_assert(sizeof(int32_t) == 4);
static_assert(sizeof(int64_t) == 8);

using size_t = uint64_t;
using uintptr_t = uint64_t;
using intptr_t = int64_t;
using ptrdiff_t = int64_t;

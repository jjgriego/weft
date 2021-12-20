export module stdint;

export using uint8_t = unsigned char;
export using uint16_t = unsigned short;
export using uint32_t = unsigned int;
export using uint64_t = unsigned long;

export using int8_t = signed char;
export using int16_t = signed short;
export using int32_t = signed int;
export using int64_t = signed long;

static_assert(sizeof(uint8_t) == 1);
static_assert(sizeof(uint16_t) == 2);
static_assert(sizeof(uint32_t) == 4);
static_assert(sizeof(uint64_t) == 8);
static_assert(sizeof(int8_t) == 1);
static_assert(sizeof(int16_t) == 2);
static_assert(sizeof(int32_t) == 4);
static_assert(sizeof(int64_t) == 8);

export using size_t = uint64_t;
export using uintptr_t = uint64_t;
export using intptr_t = int64_t;
export using ptrdiff_t = int64_t;


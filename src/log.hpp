#pragma once

#include "bytes.hpp"
#include "serial.hpp"
#include "panic.hpp"
#include "util.hpp"

namespace log_detail {

void put(uint64_t);
void put(bytes::bytes_view);

}

inline void log(bytes::bytes_view msg) {
  using namespace bytes;

  serial::write(msg);
  serial::write("\r\n"_bv);
}

template <typename T, typename ... Ts>
inline void log(bytes::bytes_view msg, T&& t, Ts&& ... args) {
  using namespace bytes;

  if (auto const j = msg.find('%')) {
    serial::write(msg.slice(0, *j));
    log_detail::put(forward<T>(t));
    return log(msg.slice(*j + 1, msg.size()), forward<Ts>(args) ...);
  } else {
    panic("unused log arguments!"_bv);
  }
}


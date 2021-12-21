#pragma once
#include "util.hpp"

namespace optional_detail {

[[noreturn]] void panic_missing_optional();

}

template <typename T>
struct optional {

  optional() = default;

  /*implicit*/ optional(T&& t)
    : m_present(true) {
    new (*m_storage) T(move(t));
  }

  operator bool() const {
    return m_present;
  }

  T* operator*() {
    if (m_present) {
      return *m_storage;
    } else {
      optional_detail::panic_missing_optional();
    }
  }
  const T* operator*() const {
    return *const_cast<optional<T>>(this);
  }

  T* operator->() { return **this; }
  const T* operator->() const { return **this; }

private:
  bool m_present{false};
  uninit<T> m_storage;
};

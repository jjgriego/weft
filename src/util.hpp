#pragma once

#include "stdint.hpp"

template <typename T> struct remove_reference      {typedef T type;};
template <typename T> struct remove_reference<T&>  {typedef T type;};
template <typename T> struct remove_reference<T&&> {typedef T type;};

template <typename T>
using remove_reference_t = typename remove_reference<T>::type;

template <typename T>
inline decltype(auto) move(T&& t) {
  return static_cast<remove_reference_t<T>&&>(t);
}

template <typename T>
inline T&& forward(remove_reference_t<T>& t) noexcept {
  return static_cast<T&&>(t);
}
template <typename T>
inline T&& forward(remove_reference_t<T>&& t) noexcept {
  return static_cast<T&&>(t);
}

template <size_t len, size_t align>
struct aligned_storage {
  uint8_t* operator*() {
    return data;
  }

  const uint8_t* operator*() const {
    return data;
  }

  alignas(align) uint8_t data[len];
};

template <typename T, typename S>
constexpr T bit_cast(const S& src) noexcept {
  T res;
  memcpy(&res, src, sizeof(S));
  return res;
}

template <typename T>
struct unaligned {
  static_assert(alignof(unaligned<T>) == alignof(char));

  /* implicit */ unaligned(T&& t) {
    *this = t;
  }

  T operator*() {
    T res;
    memcpy(&res, data, sizeof(T));
    return res;
  }

  void operator=(T&& t) {
    memcpy(data, &t, sizeof(T));
  }


  char data[sizeof(T)];
};



template <typename T>
struct uninit {
  uninit() = default;
  uninit(T&& t) {
    **this = move(t);
  }

  T* operator->() {return **this;}
  const T* operator->() const {return **this;}
  T* operator*() {
    return reinterpret_cast<T*>(*m_storage);
  }
  const T* operator*() const {
    return *const_cast<uninit<T>*>(this);
  }

private:
  aligned_storage<sizeof(T), alignof(T)> m_storage;
};

inline void* operator new(size_t /*len*/, void* buf) {
  return buf;
}


(* This runtime models values, not OCaml layouts. Its operations do not call the
   OCaml runtime; keep their semantics independent of the OCaml lowering. *)
let source =
  {cpp|#include <cmath>
#include <cfenv>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <limits>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>

namespace reference {
struct Object;
struct V {
  uint64_t bits = 0;
  std::string text;
  std::shared_ptr<Object> object;
};
struct Object {
  int tag;
  std::vector<V> fields;
};
struct Exception {
  int tag;
  V payload;
  int assertion_id = -1;
};
enum class N { I8, I16, I32, I63, I64, F32, F64 };
enum class U { Neg, Abs, Succ, Pred, Bit_not, Sqrt, Round, Trunc, Ceil, Floor };
enum class B {
  Add, Sub, Mul, Div, Rem, Min, Max, Min_num, Max_num,
  Bit_and, Bit_or, Bit_xor, Shift_left, Shift_right, Shift_right_logical,
  Eq, Ne, Lt, Le, Gt, Ge, Compare, Unsigned_compare
};

static_assert(sizeof(void*) == 8, "reference requires a 64-bit target");
static_assert(sizeof(float) == 4 && sizeof(double) == 8,
              "reference requires binary32 and binary64");
static_assert(std::numeric_limits<float>::is_iec559 &&
              std::numeric_limits<double>::is_iec559,
              "reference requires IEEE floating point");

V raw(uint64_t bits) { V v; v.bits = bits; return v; }
V boolean(bool b) { return raw(b ? 1 : 0); }
bool truth(const V& v) { return v.bits != 0; }
V string(const char* bytes, size_t length) {
  V v; v.text.assign(bytes, length); return v;
}
V aggregate(int tag, std::vector<V> fields) {
  V v; v.object = std::make_shared<Object>(Object{tag, std::move(fields)});
  return v;
}
V field(const V& v, size_t index) { return v.object->fields.at(index); }
V copy_record(const V& v) {
  return aggregate(v.object->tag, v.object->fields);
}
[[noreturn]] V raise(int tag, V payload) {
  throw Exception{tag, std::move(payload), -1};
}
[[noreturn]] V assertion(int id) { throw Exception{-1, V{}, id}; }

unsigned width(N n) {
  switch (n) {
    case N::I8: return 8;
    case N::I16: return 16;
    case N::I32: return 32;
    case N::I63: return 63;
    case N::I64: return 64;
    case N::F32: case N::F64: break;
  }
  throw std::logic_error("non-integral width");
}
bool floating(N n) { return n == N::F32 || n == N::F64; }
uint64_t mask(unsigned w) { return UINT64_MAX >> (64 - w); }
V integer(unsigned w, uint64_t bits) { return raw(bits & mask(w)); }
int64_t signed_value(unsigned w, uint64_t bits) {
  bits &= mask(w);
  // Avoid implementation-defined unsigned-to-signed narrowing.
  if (bits & (UINT64_C(1) << (w - 1)))
    return -1 - static_cast<int64_t>((~bits) & mask(w));
  return static_cast<int64_t>(bits);
}
int64_t as_int(const V& v) { return signed_value(63, v.bits); }
template<class T> T as_float(const V& v) {
  T result;
  if constexpr (sizeof(T) == 4) {
    uint32_t bits = static_cast<uint32_t>(v.bits);
    std::memcpy(&result, &bits, sizeof(result));
  } else {
    std::memcpy(&result, &v.bits, sizeof(result));
  }
  return result;
}
template<class T> V float_value(T value) {
  if constexpr (sizeof(T) == 4) {
    uint32_t bits;
    std::memcpy(&bits, &value, sizeof(bits));
    return raw(bits);
  } else {
    uint64_t bits;
    std::memcpy(&bits, &value, sizeof(bits));
    return raw(bits);
  }
}
V convert(N from, N to, const V& v) {
  if (from == to) return v;
  if (floating(to)) {
    if (floating(from)) {
      double x = from == N::F32 ? as_float<float>(v) : as_float<double>(v);
      return to == N::F32 ? float_value(static_cast<float>(x)) : float_value(x);
    }
    int64_t x = signed_value(width(from), v.bits);
    return to == N::F32 ? float_value(static_cast<float>(x))
                       : float_value(static_cast<double>(x));
  }
  unsigned w = width(to);
  if (!floating(from))
    return integer(w, static_cast<uint64_t>(signed_value(width(from), v.bits)));
  double x = from == N::F32 ? as_float<float>(v) : as_float<double>(v);
  uint64_t sign = UINT64_C(1) << (w - 1);
  double upper = std::ldexp(1.0, w - 1);
  if (std::isnan(x)) return raw(0);
  if (x >= upper) return raw(sign - 1);
  if (x < -upper) return raw(sign);
  return integer(w, static_cast<uint64_t>(static_cast<int64_t>(x)));
}
V relation(B op, int comparison) {
  switch (op) {
    case B::Eq: return boolean(comparison == 0);
    case B::Ne: return boolean(comparison != 0);
    case B::Lt: return boolean(comparison < 0);
    case B::Le: return boolean(comparison <= 0);
    case B::Gt: return boolean(comparison > 0);
    case B::Ge: return boolean(comparison >= 0);
    case B::Compare: case B::Unsigned_compare:
      return integer(63, static_cast<uint64_t>(comparison));
    default: throw std::logic_error("not a comparison");
  }
}
template<class T> int compare_float(T x, T y) {
  if (std::isnan(x)) return std::isnan(y) ? 0 : -1;
  if (std::isnan(y)) return 1;
  return (x > y) - (x < y);
}
template<class T> V float_binary(B op, T x, T y) {
  switch (op) {
    case B::Add: return float_value(T(x + y));
    case B::Sub: return float_value(T(x - y));
    case B::Mul: return float_value(T(x * y));
    case B::Div: return float_value(T(x / y));
    case B::Rem: return float_value(std::fmod(x, y));
    case B::Min: case B::Max: case B::Min_num: case B::Max_num: {
      bool minimum = op == B::Min || op == B::Min_num;
      bool number = op == B::Min_num || op == B::Max_num;
      if (std::isnan(x)) return float_value(number ? y : x);
      if (std::isnan(y)) return float_value(number ? x : y);
      if (x == 0 && y == 0) {
        bool negative = minimum ? std::signbit(x) || std::signbit(y)
                                : std::signbit(x) && std::signbit(y);
        return float_value(std::copysign(T(0), negative ? T(-1) : T(1)));
      }
      return float_value((minimum ? x < y : x > y) ? x : y);
    }
    case B::Eq: case B::Ne: case B::Lt: case B::Le: case B::Gt:
    case B::Ge: case B::Compare: return relation(op, compare_float(x, y));
    default: throw std::logic_error("non-floating operation");
  }
}
V binary(N n, B op, const V& lhs, const V& rhs) {
  if (n == N::F32)
    return float_binary(op, as_float<float>(lhs), as_float<float>(rhs));
  if (n == N::F64)
    return float_binary(op, as_float<double>(lhs), as_float<double>(rhs));
  unsigned w = width(n);
  uint64_t x = lhs.bits & mask(w), y = rhs.bits & mask(w);
  int64_t sx = signed_value(w, x), sy = signed_value(w, y);
  switch (op) {
    case B::Add: return integer(w, x + y);
    case B::Sub: return integer(w, x - y);
    case B::Mul: return integer(w, x * y);
    case B::Bit_and: return integer(w, x & y);
    case B::Bit_or: return integer(w, x | y);
    case B::Bit_xor: return integer(w, x ^ y);
    case B::Div: case B::Rem:
      if (sy == 0) throw std::logic_error("unguarded integer division by zero");
      if (x == (UINT64_C(1) << (w - 1)) && sy == -1)
        return op == B::Div ? raw(x) : raw(0);
      return integer(w, static_cast<uint64_t>(op == B::Div ? sx / sy : sx % sy));
    case B::Shift_left: case B::Shift_right: case B::Shift_right_logical: {
      int64_t count = as_int(rhs);
      if (count < 0 || count >= w) throw std::logic_error("invalid shift");
      unsigned shift = static_cast<unsigned>(count);
      if (op == B::Shift_left) return integer(w, x << shift);
      uint64_t bits = x >> shift;
      if (op == B::Shift_right && sx < 0)
        bits |= mask(w) ^ (mask(w) >> shift);
      return integer(w, bits);
    }
    case B::Unsigned_compare: return relation(op, (x > y) - (x < y));
    case B::Eq: case B::Ne: case B::Lt: case B::Le: case B::Gt:
    case B::Ge: case B::Compare: return relation(op, (sx > sy) - (sx < sy));
    default: throw std::logic_error("non-integral operation");
  }
}
template<class T> V float_unary(U op, T x) {
  switch (op) {
    case U::Neg: return float_value(T(-x));
    case U::Abs: return float_value(std::fabs(x));
    case U::Succ:
      return float_value(std::nextafter(x, std::numeric_limits<T>::infinity()));
    case U::Pred:
      return float_value(std::nextafter(x, -std::numeric_limits<T>::infinity()));
    case U::Sqrt: return float_value(std::sqrt(x));
    case U::Round: return float_value(std::round(x));
    case U::Trunc: return float_value(std::trunc(x));
    case U::Ceil: return float_value(std::ceil(x));
    case U::Floor: return float_value(std::floor(x));
    default: throw std::logic_error("non-floating unary operation");
  }
}
V unary(N n, U op, const V& v) {
  if (n == N::F32) return float_unary(op, as_float<float>(v));
  if (n == N::F64) return float_unary(op, as_float<double>(v));
  unsigned w = width(n);
  uint64_t x = v.bits;
  switch (op) {
    case U::Neg: return integer(w, UINT64_C(0) - x);
    case U::Abs:
      return integer(w, signed_value(w, x) < 0 ? UINT64_C(0) - x : x);
    case U::Succ: return integer(w, x + 1);
    case U::Pred: return integer(w, x - 1);
    case U::Bit_not: return integer(w, ~x);
    default: throw std::logic_error("non-integral unary operation");
  }
}
V compare_bool(B op, const V& x, const V& y) {
  return relation(op, (x.bits > y.bits) - (x.bits < y.bits));
}
V compare_string(B op, const V& x, const V& y) {
  int c = x.text.compare(y.text);
  return relation(op, (c > 0) - (c < 0));
}
V concat(const V& x, const V& y, size_t maximum) {
  V v; v.text = (x.text + y.text).substr(0, maximum); return v;
}
V make_array(const std::vector<size_t>& sizes, size_t depth, const V& init) {
  std::vector<V> fields;
  for (size_t i = 0; i < sizes.at(depth); ++i)
    fields.push_back(depth + 1 == sizes.size()
        ? init : make_array(sizes, depth + 1, init));
  return aggregate(0, std::move(fields));
}
void print_number(N n, const V& v) {
  if (!floating(n)) {
    std::printf("%lld ", static_cast<long long>(signed_value(width(n), v.bits)));
    return;
  }
  double x = n == N::F32 ? as_float<float>(v) : as_float<double>(v);
  if (std::isnan(x)) std::fputs("nan ", stdout);
  else if (std::isinf(x))
    std::fputs(std::signbit(x) ? "-infinity " : "infinity ", stdout);
  else std::printf("%a ", x);
}
void print_string(const V& v) {
  std::putchar('"');
  for (unsigned char c : v.text) {
    switch (c) {
      case '"': std::fputs("\\\"", stdout); break;
      case '\\': std::fputs("\\\\", stdout); break;
      case '\n': std::fputs("\\n", stdout); break;
      case '\r': std::fputs("\\r", stdout); break;
      case '\t': std::fputs("\\t", stdout); break;
      case '\b': std::fputs("\\b", stdout); break;
      default:
        if (c >= 32 && c <= 126) std::putchar(c);
        else std::printf("\\%03u", unsigned(c));
    }
  }
  std::fputs("\" ", stdout);
}
} // namespace reference
|cpp}

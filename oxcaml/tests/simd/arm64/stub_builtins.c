#include <assert.h>

#define BUILTIN(name) void name() { assert(0); }

BUILTIN(caml_neon_float64_add);
BUILTIN(caml_neon_float64_sub);
BUILTIN(caml_neon_float64_mul);
BUILTIN(caml_neon_float64_div);
BUILTIN(caml_neon_float64_max);
BUILTIN(caml_neon_float64_min);
BUILTIN(caml_neon_float64_sqrt);
BUILTIN(caml_neon_float64_round_near);


BUILTIN(caml_neon_float32x4_cmeq);
BUILTIN(caml_neon_float32x4_cmgt);
BUILTIN(caml_neon_float32x4_cmge);
BUILTIN(caml_neon_float32x4_cmle);
BUILTIN(caml_neon_float32x4_cmlt);

BUILTIN(caml_neon_float32x4_add);
BUILTIN(caml_neon_float32x4_sub);
BUILTIN(caml_neon_float32x4_mul);
BUILTIN(caml_neon_float32x4_div);
BUILTIN(caml_neon_float32x4_max);
BUILTIN(caml_neon_float32x4_min);
BUILTIN(caml_neon_float32x4_rcp);
BUILTIN(caml_neon_float32x4_rsqrt);
BUILTIN(caml_neon_float32x4_sqrt);
BUILTIN(caml_simd_vec128_high_64_to_low_64);
BUILTIN(caml_simd_vec128_low_64_to_high_64);
BUILTIN(caml_simd_vec128_interleave_high_32);


BUILTIN(caml_neon_vec128_shuffle_32);
BUILTIN(caml_neon_vec128_movemask_32);

BUILTIN(caml_neon_int8x16_add);
BUILTIN(caml_neon_int16x8_add);
BUILTIN(caml_neon_int32x4_add);
BUILTIN(caml_neon_int64x2_add);
BUILTIN(caml_neon_float64x2_add);
BUILTIN(caml_neon_int8x16_add_saturating);
BUILTIN(caml_neon_int16x8_add_saturating);
BUILTIN(caml_neon_int8x16_add_saturating_unsigned);
BUILTIN(caml_neon_int16x8_add_saturating_unsigned);
BUILTIN(caml_neon_int8x16_sub);
BUILTIN(caml_neon_int16x8_sub);
BUILTIN(caml_neon_int32x4_sub);
BUILTIN(caml_neon_int64x2_sub);
BUILTIN(caml_neon_float64x2_sub);
BUILTIN(caml_neon_int8x16_sub_saturating);
BUILTIN(caml_neon_int16x8_sub_saturating);
BUILTIN(caml_neon_int8x16_sub_saturating_unsigned);
BUILTIN(caml_neon_int16x8_sub_saturating_unsigned);
BUILTIN(caml_neon_int8x16_max_unsigned);
BUILTIN(caml_neon_int16x8_max);
BUILTIN(caml_neon_float64x2_max);
BUILTIN(caml_neon_int8x16_min_unsigned);
BUILTIN(caml_neon_int16x8_min);
BUILTIN(caml_neon_float64x2_min);
BUILTIN(caml_neon_float64x2_mul);
BUILTIN(caml_neon_float64x2_div);
BUILTIN(caml_neon_float64x2_sqrt);
BUILTIN(caml_neon_vec128_and);
BUILTIN(caml_neon_vec128_andnot);
BUILTIN(caml_neon_vec128_or);
BUILTIN(caml_neon_vec128_xor);
BUILTIN(caml_neon_vec128_movemask_8);
BUILTIN(caml_neon_vec128_movemask_64);
BUILTIN(caml_neon_vec128_shift_left_bytes);
BUILTIN(caml_neon_vec128_shift_right_bytes);
BUILTIN(caml_neon_int8x16_cmpeq);
BUILTIN(caml_neon_int16x8_cmpeq);
BUILTIN(caml_neon_int32x4_cmpeq);
BUILTIN(caml_neon_int8x16_cmpgt);
BUILTIN(caml_neon_int16x8_cmpgt);
BUILTIN(caml_neon_int32x4_cmpgt);
BUILTIN(caml_neon_float64x2_cmp);
BUILTIN(caml_neon_cvt_int32x2_to_float64x2);
BUILTIN(caml_neon_cvt_int32x4_to_float32x4);
BUILTIN(caml_neon_cvt_float64x2_to_float32x2);
BUILTIN(caml_neon_cvt_float32x4_to_int32x4);
BUILTIN(caml_neon_cvtt_float32x4_to_int32x4);
BUILTIN(caml_neon_cvt_float32x2_to_float64x2);

BUILTIN(caml_neon_int32x4_bitwise_not);
BUILTIN(caml_neon_int32x4_bitwise_or);
BUILTIN(caml_neon_int32x4_bitwise_and);
BUILTIN(caml_neon_int32x4_bitwise_xor);
BUILTIN(caml_neon_int32x4_neg);

BUILTIN(caml_neon_int32x4_cmpeqz);
BUILTIN(caml_neon_int32x4_cmpgez);
BUILTIN(caml_neon_int32x4_cmpgtz);
BUILTIN(caml_neon_int32x4_cmplez);
BUILTIN(caml_neon_int32x4_cmpltz);
BUILTIN(caml_neon_int32x4_cnt);

BUILTIN(caml_neon_int16x8_cmpeqz);
BUILTIN(caml_neon_int16x8_cmpgez);
BUILTIN(caml_neon_int16x8_cmpgtz);
BUILTIN(caml_neon_int16x8_cmplez);
BUILTIN(caml_neon_int16x8_cmpltz);

BUILTIN(caml_neon_int8x16_cmpeqz);
BUILTIN(caml_neon_int8x16_cmpgez);
BUILTIN(caml_neon_int8x16_cmpgtz);
BUILTIN(caml_neon_int8x16_cmplez);
BUILTIN(caml_neon_int8x16_cmpltz);

BUILTIN(caml_neon_int8x16_neg);
BUILTIN(caml_neon_int8x16_sll);
BUILTIN(caml_neon_int8x16_slli);
BUILTIN(caml_neon_int8x16_srai);
BUILTIN(caml_neon_int8x16_srli);
BUILTIN(caml_neon_int8x16_sshl);
BUILTIN(caml_neon_int8x16_ushl);



BUILTIN(caml_neon_int16x8_sll);
BUILTIN(caml_neon_int32x4_sll);
BUILTIN(caml_neon_int64x2_sll);
BUILTIN(caml_neon_int16x8_srl);
BUILTIN(caml_neon_int32x4_srl);
BUILTIN(caml_neon_int64x2_srl);
BUILTIN(caml_neon_int16x8_sra);
BUILTIN(caml_neon_int32x4_sra);
BUILTIN(caml_neon_int16x8_slli);
BUILTIN(caml_neon_int32x4_slli);
BUILTIN(caml_neon_int64x2_slli);
BUILTIN(caml_neon_int16x8_srli);
BUILTIN(caml_neon_int32x4_srli);
BUILTIN(caml_neon_int64x2_srli);
BUILTIN(caml_neon_int16x8_srai);
BUILTIN(caml_neon_int32x4_srai);
BUILTIN(caml_neon_vec128_shuffle_64);
BUILTIN(caml_neon_vec128_shuffle_high_16);
BUILTIN(caml_neon_vec128_shuffle_low_16);
BUILTIN(caml_simd_vec128_interleave_high_8);
BUILTIN(caml_simd_vec128_interleave_low_8);
BUILTIN(caml_simd_vec128_interleave_high_16);
BUILTIN(caml_simd_vec128_interleave_low_16);
BUILTIN(caml_simd_vec128_interleave_high_64);
BUILTIN(caml_neon_cvt_int16x8_int8x16_saturating);
BUILTIN(caml_neon_cvt_int32x4_int16x8_saturating);
BUILTIN(caml_neon_cvt_int16x8_int8x16_saturating_unsigned);
BUILTIN(caml_neon_cvt_int32x4_int16x8_saturating_unsigned);
BUILTIN(caml_neon_int16x8_mul_high);
BUILTIN(caml_neon_int16x8_mul_high_unsigned);
BUILTIN(caml_neon_int16x8_mul_low);


BUILTIN(caml_neon_float32x4_hadd);
BUILTIN(caml_neon_float64x2_hadd);
BUILTIN(caml_neon_float32x4_hsub);
BUILTIN(caml_neon_float64x2_hsub);
BUILTIN(caml_neon_vec128_dup_low_64);
BUILTIN(caml_neon_vec128_dup_odd_32);
BUILTIN(caml_neon_vec128_dup_even_32);

BUILTIN(caml_neon_int8x16_abs);
BUILTIN(caml_neon_int16x8_abs);
BUILTIN(caml_neon_int32x4_abs);
BUILTIN(caml_neon_int16x8_hadd);
BUILTIN(caml_neon_int32x4_hadd);

BUILTIN(caml_neon_int16x8_hsub);
BUILTIN(caml_neon_int32x4_hsub);
BUILTIN(caml_neon_int16x8_hsub_saturating);
BUILTIN(caml_neon_int8x16_mulsign);
BUILTIN(caml_neon_int16x8_mulsign);
BUILTIN(caml_neon_int32x4_mulsign);
BUILTIN(caml_neon_vec128_shuffle_8);
BUILTIN(caml_neon_vec128_align_right_bytes);


BUILTIN(caml_neon_vec128_blend_16);
BUILTIN(caml_neon_vec128_blend_32);
BUILTIN(caml_neon_vec128_blend_64);
BUILTIN(caml_neon_vec128_blendv_8);
BUILTIN(caml_neon_vec128_blendv_32);
BUILTIN(caml_neon_vec128_blendv_64);
BUILTIN(caml_neon_int64x2_cmpeq);
BUILTIN(caml_neon_cvtsx_int8x16_to_int16x8);
BUILTIN(caml_neon_cvtsx_int16x8_to_int32x4);
BUILTIN(caml_neon_cvtzx_int8x16_to_int16x8);
BUILTIN(caml_neon_cvtzx_int16x8_to_int32x4);
BUILTIN(caml_neon_float32x4_dp);
BUILTIN(caml_neon_float64x2_dp);
BUILTIN(caml_neon_int8x16_extract);
BUILTIN(caml_neon_int16x8_extract);
BUILTIN(caml_neon_int32x4_extract);
BUILTIN(caml_neon_int64x2_extract);
BUILTIN(caml_neon_int8x16_insert);
BUILTIN(caml_neon_int16x8_insert);
BUILTIN(caml_neon_int32x4_insert);
BUILTIN(caml_neon_int64x2_insert);
BUILTIN(caml_neon_float32x4_round_near);
BUILTIN(caml_neon_float64x2_round);
BUILTIN(caml_neon_int8x16_max);
BUILTIN(caml_neon_int32x4_max);
BUILTIN(caml_neon_int16x8_max_unsigned);
BUILTIN(caml_neon_int32x4_max_unsigned);
BUILTIN(caml_neon_int8x16_min);
BUILTIN(caml_neon_int32x4_min);
BUILTIN(caml_neon_int16x8_min_unsigned);
BUILTIN(caml_neon_int32x4_min_unsigned);
BUILTIN(caml_neon_int32x4_mul_low);

BUILTIN(caml_neon_int64x2_cmpgt);

BUILTIN(caml_neon_clmul_int64x2);
BUILTIN(caml_neon_int64_extract_bits);
BUILTIN(caml_neon_int64_deposit_bits);


BUILTIN(caml_neon_cvt_float64x2_to_int64x2)
BUILTIN(caml_neon_cvtt_float64x2_to_int64x2)
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_saturating)
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_saturating_unsigned)
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_saturating)
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_saturating_unsigned)
BUILTIN(caml_neon_cvt_int64x2_to_float64x2)
BUILTIN(caml_neon_cvtsx_int32x4_to_int64x2)
BUILTIN(caml_neon_cvtzx_int32x4_to_int64x2)
BUILTIN(caml_neon_float64x2_cmeq)
BUILTIN(caml_neon_float64x2_cmge)
BUILTIN(caml_neon_float64x2_cmgt)
BUILTIN(caml_neon_float64x2_cmle)
BUILTIN(caml_neon_float64x2_cmlt)
BUILTIN(caml_neon_float64x2_round_near)
BUILTIN(caml_neon_float64x2_round_current)
BUILTIN(caml_neon_int16x8_neg)
BUILTIN(caml_neon_int16x8_sshl)
BUILTIN(caml_neon_int16x8_ushl)
BUILTIN(caml_neon_int32x4_dup)
BUILTIN(caml_neon_int32x4_dup_lane)
BUILTIN(caml_neon_int32x4_sshl)
BUILTIN(caml_neon_int32x4_ushl)
BUILTIN(caml_neon_int64x2_bitwise_and)
BUILTIN(caml_neon_int64x2_bitwise_not)
BUILTIN(caml_neon_int64x2_bitwise_or)
BUILTIN(caml_neon_int64x2_bitwise_xor)
BUILTIN(caml_neon_int64x2_cmpeqz)
BUILTIN(caml_neon_int64x2_cmpgez)
BUILTIN(caml_neon_int64x2_cmpgtz)
BUILTIN(caml_neon_int64x2_cmplez)
BUILTIN(caml_neon_int64x2_cmpltz)
BUILTIN(caml_neon_int64x2_dup)
BUILTIN(caml_neon_int64x2_dup_lane)
BUILTIN(caml_neon_int64x2_neg)
BUILTIN(caml_neon_int64x2_sshl)
BUILTIN(caml_neon_int64x2_ushl)
BUILTIN(caml_neon_cvt_int64x2_to_int32x4)

BUILTIN(caml_neon_int8x16_bitwise_and)
BUILTIN(caml_neon_int8x16_bitwise_not)
BUILTIN(caml_neon_int8x16_bitwise_or)
BUILTIN(caml_neon_int8x16_bitwise_xor)
BUILTIN(caml_neon_int8x16_ext)

BUILTIN(caml_neon_cvt_int64x2_to_int32x4_high_saturating)
BUILTIN(caml_neon_cvt_int64x2_to_int32x4_low_saturating)
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_high_saturating);
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_high_saturating_unsigned);
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_low_saturating);
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_low_saturating_unsigned);

BUILTIN(caml_neon_cvt_int16x8_to_int8x16_high_saturating);
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_high_saturating_unsigned);
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_low_saturating);
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_low_saturating_unsigned);


BUILTIN(caml_neon_cvt_int16x8_to_int8x16_high);
BUILTIN(caml_neon_cvt_int16x8_to_int8x16_low);
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_high);
BUILTIN(caml_neon_cvt_int32x4_to_int16x8_low);
BUILTIN(caml_neon_int16x8_dup);
BUILTIN(caml_neon_int16x8_dup_lane);
BUILTIN(caml_neon_int16x8_minpos_unsigned);
BUILTIN(caml_neon_int16x8_mul_high_long);
BUILTIN(caml_neon_int16x8_mul_high_long_unsigned);
BUILTIN(caml_neon_int16x8_mul_low_long);
BUILTIN(caml_neon_int16x8_mul_low_long_unsigned);
BUILTIN(caml_neon_int8x16_dup);
BUILTIN(caml_neon_int8x16_dup_lane);

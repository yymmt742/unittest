    procedure,private :: utest_almost_equal_byte_00
    procedure,private :: utest_almost_equal_byte_01
    procedure,private :: utest_almost_equal_byte_10
    procedure,private :: utest_almost_equal_byte_11
    procedure,private :: utest_almost_equal_int2_00
    procedure,private :: utest_almost_equal_int2_01
    procedure,private :: utest_almost_equal_int2_10
    procedure,private :: utest_almost_equal_int2_11
    procedure,private :: utest_almost_equal_int4_00
    procedure,private :: utest_almost_equal_int4_01
    procedure,private :: utest_almost_equal_int4_10
    procedure,private :: utest_almost_equal_int4_11
    procedure,private :: utest_almost_equal_int8_00
    procedure,private :: utest_almost_equal_int8_01
    procedure,private :: utest_almost_equal_int8_10
    procedure,private :: utest_almost_equal_int8_11
    procedure,private :: utest_almost_equal_real_00
    procedure,private :: utest_almost_equal_real_01
    procedure,private :: utest_almost_equal_real_10
    procedure,private :: utest_almost_equal_real_11
    procedure,private :: utest_almost_equal_dble_00
    procedure,private :: utest_almost_equal_dble_01
    procedure,private :: utest_almost_equal_dble_10
    procedure,private :: utest_almost_equal_dble_11
    procedure,private :: utest_almost_equal_quad_00
    procedure,private :: utest_almost_equal_quad_01
    procedure,private :: utest_almost_equal_quad_10
    procedure,private :: utest_almost_equal_quad_11
    procedure, private :: utest_almost_equal_cmp4_00
    procedure, private :: utest_almost_equal_cmp4_01
    procedure, private :: utest_almost_equal_cmp4_10
    procedure, private :: utest_almost_equal_cmp4_11
    procedure, private :: utest_almost_equal_cmp8_00
    procedure, private :: utest_almost_equal_cmp8_01
    procedure, private :: utest_almost_equal_cmp8_10
    procedure, private :: utest_almost_equal_cmp8_11
    procedure, private :: utest_almost_equal_cmp16_00
    procedure, private :: utest_almost_equal_cmp16_01
    procedure, private :: utest_almost_equal_cmp16_10
    procedure, private :: utest_almost_equal_cmp16_11
    generic           :: assert_almost_equal =>      &
                       & utest_almost_equal_byte_00, &
                       & utest_almost_equal_byte_01, &
                       & utest_almost_equal_byte_10, &
                       & utest_almost_equal_byte_11, &
                       & utest_almost_equal_int2_00, &
                       & utest_almost_equal_int2_01, &
                       & utest_almost_equal_int2_10, &
                       & utest_almost_equal_int2_11, &
                       & utest_almost_equal_int4_00, &
                       & utest_almost_equal_int4_01, &
                       & utest_almost_equal_int4_10, &
                       & utest_almost_equal_int4_11, &
                       & utest_almost_equal_int8_00, &
                       & utest_almost_equal_int8_01, &
                       & utest_almost_equal_int8_10, &
                       & utest_almost_equal_int8_11, &
                       & utest_almost_equal_real_00, &
                       & utest_almost_equal_real_01, &
                       & utest_almost_equal_real_10, &
                       & utest_almost_equal_real_11, &
                       & utest_almost_equal_dble_00, &
                       & utest_almost_equal_dble_01, &
                       & utest_almost_equal_dble_10, &
                       & utest_almost_equal_dble_11, &
                       & utest_almost_equal_quad_00, &
                       & utest_almost_equal_quad_01, &
                       & utest_almost_equal_quad_10, &
                       & utest_almost_equal_quad_11, &
                       & utest_almost_equal_cmp4_00, &
                       & utest_almost_equal_cmp4_01, &
                       & utest_almost_equal_cmp4_10, &
                       & utest_almost_equal_cmp4_11, &
                       & utest_almost_equal_cmp8_00, &
                       & utest_almost_equal_cmp8_01, &
                       & utest_almost_equal_cmp8_10, &
                       & utest_almost_equal_cmp8_11, &
                       & utest_almost_equal_cmp16_00, &
                       & utest_almost_equal_cmp16_01, &
                       & utest_almost_equal_cmp16_10, &
                       & utest_almost_equal_cmp16_11
    procedure, private :: utest_not_almost_equal_byte_00
    procedure, private :: utest_not_almost_equal_byte_01
    procedure, private :: utest_not_almost_equal_byte_10
    procedure, private :: utest_not_almost_equal_byte_11
    procedure, private :: utest_not_almost_equal_int2_00
    procedure, private :: utest_not_almost_equal_int2_01
    procedure, private :: utest_not_almost_equal_int2_10
    procedure, private :: utest_not_almost_equal_int2_11
    procedure, private :: utest_not_almost_equal_int4_00
    procedure, private :: utest_not_almost_equal_int4_01
    procedure, private :: utest_not_almost_equal_int4_10
    procedure, private :: utest_not_almost_equal_int4_11
    procedure, private :: utest_not_almost_equal_int8_00
    procedure, private :: utest_not_almost_equal_int8_01
    procedure, private :: utest_not_almost_equal_int8_10
    procedure, private :: utest_not_almost_equal_int8_11
    procedure, private :: utest_not_almost_equal_real_00
    procedure, private :: utest_not_almost_equal_real_01
    procedure, private :: utest_not_almost_equal_real_10
    procedure, private :: utest_not_almost_equal_real_11
    procedure, private :: utest_not_almost_equal_dble_00
    procedure, private :: utest_not_almost_equal_dble_01
    procedure, private :: utest_not_almost_equal_dble_10
    procedure, private :: utest_not_almost_equal_dble_11
    procedure, private :: utest_not_almost_equal_quad_00
    procedure, private :: utest_not_almost_equal_quad_01
    procedure, private :: utest_not_almost_equal_quad_10
    procedure, private :: utest_not_almost_equal_quad_11
    procedure, private :: utest_not_almost_equal_cmp4_00
    procedure, private :: utest_not_almost_equal_cmp4_01
    procedure, private :: utest_not_almost_equal_cmp4_10
    procedure, private :: utest_not_almost_equal_cmp4_11
    procedure, private :: utest_not_almost_equal_cmp8_00
    procedure, private :: utest_not_almost_equal_cmp8_01
    procedure, private :: utest_not_almost_equal_cmp8_10
    procedure, private :: utest_not_almost_equal_cmp8_11
    procedure, private :: utest_not_almost_equal_cmp16_00
    procedure, private :: utest_not_almost_equal_cmp16_01
    procedure, private :: utest_not_almost_equal_cmp16_10
    procedure, private :: utest_not_almost_equal_cmp16_11
    generic           :: assert_not_almost_equal =>      &
                       & utest_not_almost_equal_byte_00, &
                       & utest_not_almost_equal_byte_01, &
                       & utest_not_almost_equal_byte_10, &
                       & utest_not_almost_equal_byte_11, &
                       & utest_not_almost_equal_int2_00, &
                       & utest_not_almost_equal_int2_01, &
                       & utest_not_almost_equal_int2_10, &
                       & utest_not_almost_equal_int2_11, &
                       & utest_not_almost_equal_int4_00, &
                       & utest_not_almost_equal_int4_01, &
                       & utest_not_almost_equal_int4_10, &
                       & utest_not_almost_equal_int4_11, &
                       & utest_not_almost_equal_int8_00, &
                       & utest_not_almost_equal_int8_01, &
                       & utest_not_almost_equal_int8_10, &
                       & utest_not_almost_equal_int8_11, &
                       & utest_not_almost_equal_real_00, &
                       & utest_not_almost_equal_real_01, &
                       & utest_not_almost_equal_real_10, &
                       & utest_not_almost_equal_real_11, &
                       & utest_not_almost_equal_dble_00, &
                       & utest_not_almost_equal_dble_01, &
                       & utest_not_almost_equal_dble_10, &
                       & utest_not_almost_equal_dble_11, &
                       & utest_not_almost_equal_quad_00, &
                       & utest_not_almost_equal_quad_01, &
                       & utest_not_almost_equal_quad_10, &
                       & utest_not_almost_equal_quad_11, &
                       & utest_not_almost_equal_cmp4_00, &
                       & utest_not_almost_equal_cmp4_01, &
                       & utest_not_almost_equal_cmp4_10, &
                       & utest_not_almost_equal_cmp4_11, &
                       & utest_not_almost_equal_cmp8_00, &
                       & utest_not_almost_equal_cmp8_01, &
                       & utest_not_almost_equal_cmp8_10, &
                       & utest_not_almost_equal_cmp8_11, &
                       & utest_not_almost_equal_cmp16_00, &
                       & utest_not_almost_equal_cmp16_01, &
                       & utest_not_almost_equal_cmp16_10, &
                       & utest_not_almost_equal_cmp16_11

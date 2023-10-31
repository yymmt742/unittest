    procedure,private :: utest_equal_byte_00
    procedure,private :: utest_equal_byte_01
    procedure,private :: utest_equal_byte_10
    procedure,private :: utest_equal_byte_11
    procedure,private :: utest_equal_int2_00
    procedure,private :: utest_equal_int2_01
    procedure,private :: utest_equal_int2_10
    procedure,private :: utest_equal_int2_11
    procedure,private :: utest_equal_int4_00
    procedure,private :: utest_equal_int4_01
    procedure,private :: utest_equal_int4_10
    procedure,private :: utest_equal_int4_11
    procedure,private :: utest_equal_int8_00
    procedure,private :: utest_equal_int8_01
    procedure,private :: utest_equal_int8_10
    procedure,private :: utest_equal_int8_11
    procedure,private :: utest_equal_char_00
    procedure,private :: utest_equal_char_01
    procedure,private :: utest_equal_char_10
    procedure,private :: utest_equal_char_11
    procedure,private :: utest_equal_logi_00
    procedure,private :: utest_equal_logi_01
    procedure,private :: utest_equal_logi_10
    procedure,private :: utest_equal_logi_11
    generic           :: assert_equal =>      &
                       & utest_equal_byte_00, &
                       & utest_equal_byte_01, &
                       & utest_equal_byte_10, &
                       & utest_equal_byte_11, &
                       & utest_equal_int2_00, &
                       & utest_equal_int2_01, &
                       & utest_equal_int2_10, &
                       & utest_equal_int2_11, &
                       & utest_equal_int4_00, &
                       & utest_equal_int4_01, &
                       & utest_equal_int4_10, &
                       & utest_equal_int4_11, &
                       & utest_equal_int8_00, &
                       & utest_equal_int8_01, &
                       & utest_equal_int8_10, &
                       & utest_equal_int8_11, &
                       & utest_equal_char_00, &
                       & utest_equal_char_01, &
                       & utest_equal_char_10, &
                       & utest_equal_char_11, &
                       & utest_equal_logi_00, &
                       & utest_equal_logi_01, &
                       & utest_equal_logi_10, &
                       & utest_equal_logi_11
    procedure,private :: utest_not_equal_byte_00
    procedure,private :: utest_not_equal_byte_01
    procedure,private :: utest_not_equal_byte_10
    procedure,private :: utest_not_equal_byte_11
    procedure,private :: utest_not_equal_int2_00
    procedure,private :: utest_not_equal_int2_01
    procedure,private :: utest_not_equal_int2_10
    procedure,private :: utest_not_equal_int2_11
    procedure,private :: utest_not_equal_int4_00
    procedure,private :: utest_not_equal_int4_01
    procedure,private :: utest_not_equal_int4_10
    procedure,private :: utest_not_equal_int4_11
    procedure,private :: utest_not_equal_int8_00
    procedure,private :: utest_not_equal_int8_01
    procedure,private :: utest_not_equal_int8_10
    procedure,private :: utest_not_equal_int8_11
    procedure,private :: utest_not_equal_char_00
    procedure,private :: utest_not_equal_char_01
    procedure,private :: utest_not_equal_char_10
    procedure,private :: utest_not_equal_char_11
    procedure,private :: utest_not_equal_logi_00
    procedure,private :: utest_not_equal_logi_01
    procedure,private :: utest_not_equal_logi_10
    procedure,private :: utest_not_equal_logi_11
    generic           :: assert_not_equal =>      &
                       & utest_not_equal_byte_00, &
                       & utest_not_equal_byte_01, &
                       & utest_not_equal_byte_10, &
                       & utest_not_equal_byte_11, &
                       & utest_not_equal_int2_00, &
                       & utest_not_equal_int2_01, &
                       & utest_not_equal_int2_10, &
                       & utest_not_equal_int2_11, &
                       & utest_not_equal_int4_00, &
                       & utest_not_equal_int4_01, &
                       & utest_not_equal_int4_10, &
                       & utest_not_equal_int4_11, &
                       & utest_not_equal_int8_00, &
                       & utest_not_equal_int8_01, &
                       & utest_not_equal_int8_10, &
                       & utest_not_equal_int8_11, &
                       & utest_not_equal_char_00, &
                       & utest_not_equal_char_01, &
                       & utest_not_equal_char_10, &
                       & utest_not_equal_char_11, &
                       & utest_not_equal_logi_00, &
                       & utest_not_equal_logi_01, &
                       & utest_not_equal_logi_10, &
                       & utest_not_equal_logi_11

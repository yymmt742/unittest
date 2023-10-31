    procedure,private :: utest_assert_0
    procedure,private :: utest_assert_1
    generic           :: assert  =>      &
                       & utest_assert_0, &
                       & utest_assert_1
    procedure,private :: utest_assert_true_0
    procedure,private :: utest_assert_true_1
    generic           :: assert_true  =>      &
                       & utest_assert_true_0, &
                       & utest_assert_true_1
    procedure,private :: utest_assert_false_0
    procedure,private :: utest_assert_false_1
    generic           :: assert_false  =>      &
                       & utest_assert_false_0, &
                       & utest_assert_false_1

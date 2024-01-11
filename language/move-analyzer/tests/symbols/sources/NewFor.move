module Symbols::NewFor { 
    public fun new_for_test(v: u64):u64 {
        let ret = 0;
        for (i in 0..v) {
            ret = ret + i;
        };
        ret
    }
}
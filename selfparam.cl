class Main {
    -- Should FAIL: 'self' cannot be a parameter name
    sparam(self: Int): Int { 
        self 
    };
    
    main(): Int { 0 };
};

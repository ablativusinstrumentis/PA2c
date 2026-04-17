class A { };
class B inherits A {
    join_test(c: C): B {
        if true then self else c fi
    };
};
class C inherits A { };

class Main { main(): Int { 0 }; };

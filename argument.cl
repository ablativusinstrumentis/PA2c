class A {};
class B inherits A {};

class Main {
    compute(b : B) : Int { 1 };
    a : A <- new B;

    main() : Int {
        compute(a)
    };
};

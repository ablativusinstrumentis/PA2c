class A { f(): Int { 1 }; };
class B { f(): Int { 2 }; };
class C inherits A { };

class Main {
    c: C <- new C;
    main(): Int {
        c@B.f()
    };
};

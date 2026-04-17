class A { f(): Int { 0 }; };
class B inherits A {};
class C inherits B {};

class Main {
    c: C <- new C;
    main(): Int {
        c@A.f() 
    };
};

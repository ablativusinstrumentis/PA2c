class A {};
class B inherits A {};
class C inherits A {};

class Main {
    b : B <- new B;
    a : A <- new A;

    main() : Object {
        {
            b <- new C; 
            b <- a; 
        }
    };
};

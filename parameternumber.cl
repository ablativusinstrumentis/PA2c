class Parent {
    foo(x: Int): Int { x };
};

class Child inherits Parent {
    foo(x: Int, y: Int): Int { x + y };
};

class Main { main(): Int { 0 }; };

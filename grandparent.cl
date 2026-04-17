class Grandparent { };
class Parent1 inherits Grandparent { };
class Parent2 inherits Grandparent { };
class Child inherits Parent1 { };

class Main {
    main(): Grandparent {
        case 5 of
            x: Int => new Child;
            y: String => new Parent2;
            z: Bool => new Parent1;
        esac
    };
};

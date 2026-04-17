class A { };
class B inherits A { };

class Main inherits A {
    main(): Object {
        let x: A <- (case "hi" of 
                        s: String => self; 
                        i: Int => new B; 
                    esac) 
        in x
    };
};

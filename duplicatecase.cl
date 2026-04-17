class Main inherits IO {
    main() : Int {
        1
    };

    bad(obj : Object) : Int {
        case obj of
            x : Int => 1;
            y : Int => 2;
            z : Object => 0;
        esac
    };
};

class Alpha {
    get(): Object { new Object };
};

class Beta inherits Alpha {
    get(): String { "hello" };
};

class Main { main(): Int { 0 }; };

class Main {
    main(): Object {
        -- Should FAIL: 'self' cannot be the target of an assignment.
        self <- new Main
    };
};

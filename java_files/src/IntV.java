class IntV extends Value {
    /* Fields */
    int iv;

    /* Constructors */
    public IntV (int i) {
	iv = i;
    }

    int get_int() {
        return iv;
    }

    @Override
    void print_value() {
        System.out.print(iv);
    }
}

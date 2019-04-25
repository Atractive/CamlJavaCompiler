class Quote extends Instr {
    private final Value v;

    /* Constructors */
    public Quote (Value vl) {
        v = vl;
    }
    
    void exec_instr(Config cf) {
        cf.set_value(v);
        cf.get_code().pop();
    }
}

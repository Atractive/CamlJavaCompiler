import java.util.*;

class Config extends Object {
    private Value v;
    private LinkedList<Instr> c;
    private LinkedList<StackElem> s;

    /*
        L'environnement est sous la forme d'une liste de couples:
        -1er élément du couple : le nom de la fonction
        -2nd élément du couple : le code de la fonction compilé
    */
    private LinkedList<Pair<String, LinkedList<Instr>>> env;

    Value get_value() {
        return v;
    }
    LinkedList<Instr> get_code() {
        return c;
    }
    LinkedList<StackElem> get_stack() {
        return s;
    }
    
    LinkedList<Pair<String, LinkedList<Instr>>> get_env() {
        return env;
    }

    void set_value(Value nv) {
        v = nv;
    }
    void set_code(LinkedList<Instr> nc) {
        c = nc;
    }
    void set_stack(LinkedList<StackElem> ns) {
        s = ns;
    }
    public void set_env(LinkedList<Pair<String, LinkedList<Instr>>> ne) {
        env = ne;
    }

    /* Constructors */
    public Config (Value vl, LinkedList<Instr> cd, LinkedList<StackElem> se, LinkedList<Pair<String, LinkedList<Instr>>> env) {
        v = vl;
        c = cd;
        s = se;
        this.env = env;
    }

    // one-step execution 
    boolean exec_step() {
        
        if (c.isEmpty()){
            return false;
        }
        else{
            c.get(0).exec_instr(this);
            return true;
        }
        
    }

    // run to completion
    void exec() {
        while (exec_step()){}
    }

    // run for n steps
    void step(int n) {
        for(int i=0; i<n && exec_step(); n++){}
    }
    
}


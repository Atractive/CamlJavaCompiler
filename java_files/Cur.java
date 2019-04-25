import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class Cur extends Instr{
    LinkedList<Instr> code;
    
    public Cur(LinkedList<Instr> code){
        this.code = code;
    }

    @Override
    void exec_instr(Config cf) {
        cf.set_value(new ClosureV(code, cf.get_value()));
        cf.get_code().pop();
    }
    
}

/**
 *
 * @author Benjamin Bardy
 */
public class Cons extends Instr{

    @Override
    void exec_instr(Config cf) {
        //1er élément du stack
        ValueSE y = (ValueSE)cf.get_stack().pop();
        
        //Paire (y, valeur)
        cf.set_value(new PairV(y.get_valeur(), cf.get_value()));
        cf.get_code().pop();
    }
    
    
    
}

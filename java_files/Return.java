/**
 *
 * @author Benjamin Bardy
 */
public class Return extends Instr{

    @Override
    void exec_instr(Config cf) {
        //On réutilise le code qui était au sommet du stack
        cf.set_code(((CodeSE)cf.get_stack().pop()).get_code());
    }
    
    
}

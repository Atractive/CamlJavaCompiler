/**
 *
 * @author Benjamin Bardy
 */
public class Swap extends Instr{

    @Override
    void exec_instr(Config cf) {
        Value x = cf.get_value();
        
        //Valeur au sommet du stack
        Value y = ((ValueSE)cf.get_stack().pop()).get_valeur();
        
        //Permutation des deux valeurs
        cf.set_value(y);
        cf.get_stack().addFirst(new ValueSE(x));
        
        cf.get_code().pop();
    }
    
    
    
}

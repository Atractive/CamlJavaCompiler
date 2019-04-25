import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class App extends Instr{

    @Override
    void exec_instr(Config cf) {
        cf.get_code().pop();
        
        //On récupère la paire (ClosureV(code, valeur), Value)
        PairV pair = (PairV)cf.get_value();
        
        //On récupère le closure
        ClosureV closure = (ClosureV) pair.get_first();
    
        //Nouvelle paire (valeur, Value) pour valeur
        cf.set_value(new PairV(closure.get_valeur(), pair.get_snd()));
        
        //On met le code de la config sur le stack
        cf.get_stack().addFirst(new CodeSE(cf.get_code()));
        
        //Utilisation du code du Closure
        cf.set_code(new LinkedList<>(closure.get_code()));
    }
    
}

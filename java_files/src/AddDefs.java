import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class AddDefs extends Instr{
    private final LinkedList<Pair<String, LinkedList<Instr>>> defs;
    
    public AddDefs(LinkedList<Pair<String, LinkedList<Instr>>> defs){
        this.defs = defs;
    }
    
    @Override
    void exec_instr(Config cf) {
        //L'environnement est une liste de paires (Nom de la fonction, son code compilé)
        LinkedList<Pair<String, LinkedList<Instr>>> env = cf.get_env();
        
        //On ajoute les définitions au début de la liste (soit au sommet de la pile)
        defs.addAll(env);
        
        cf.set_env(defs);
        
        cf.get_code().pop();
    }
    
}

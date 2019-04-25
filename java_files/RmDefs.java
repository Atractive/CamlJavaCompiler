import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class RmDefs extends Instr{
    private final int n;
    
    public RmDefs(int n){
        this.n = n;
    }

    @Override
    void exec_instr(Config cf) {
        LinkedList<Pair<String, LinkedList<Instr>>> env = cf.get_env();
        
        //Enlevement des n premières fonctions de l'environnement
        for(int i=0; i<n ; i++){
            env.pop();
        }
        
        cf.set_env(env);
        cf.get_code().pop();
    }
}

import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class Call extends Instr{
    private final String functionName;
    
    public Call(String functionName){
        this.functionName = functionName;
    }

    @Override
    void exec_instr(Config cf) {
        cf.get_code().pop();
        
        //Recherche du code compilé de la fonction dans l'environnement
        for(Pair<String, LinkedList<Instr>> pair : cf.get_env()){
            if(functionName.equals(pair.get_first())){
                //Copie du code de la fonction
                LinkedList<Instr> function_prog = new LinkedList<>(pair.get_second());
                
                //Auquel on ajoute le code déjà présent
                function_prog.addAll(cf.get_code());
                
                cf.set_code(function_prog);
                break;
            }
        }
        
    }
    
}

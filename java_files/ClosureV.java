import java.util.LinkedList;

/**
 *
 * @author Benjamin Bardy
 */
public class ClosureV extends Value{
    private final LinkedList<Instr> code;
    private final Value valeur;
    
    public ClosureV(LinkedList<Instr> code, Value valeur){
        this.code = code;
        this.valeur = valeur;
    }

    public LinkedList<Instr> get_code() {
        return code;
    }

    public Value get_valeur() {
        return valeur;
    }

    
    
    
}

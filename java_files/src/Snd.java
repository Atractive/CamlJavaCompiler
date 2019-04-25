/**
 *
 * @author Benjamin Bardy
 */
public class Snd extends Instr{
    
    @Override
    void exec_instr(Config cf){
        //Récupération du 2nd élément du couple
        cf.set_value(((PairV)(cf.get_value())).get_snd());
        cf.get_code().pop();
    }
    
}

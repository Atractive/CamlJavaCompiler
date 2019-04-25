// compile with: javac *java
// run with: java Main

public class Main {
    
    public static void main(String[] args){

        Config cfg;

        cfg = new Config(new NullV(), Gen.code, LLE.empty(), LLE.empty());
        cfg.exec();
        cfg.get_value().print_value();
        System.out.println();
        
    }

}
